import type { NextPage } from 'next'
import * as XLSX from 'xlsx'

// Notes:
// Generalize toInt?

(async () => {
    console.log("begin", performance.now());
    const url = "http://homelab:3000/RADR-1202.xlsx";
    const data = await (await fetch(url)).arrayBuffer();
    let workbook: XLSX.WorkBook = XLSX.read(data);
    
    // Test: Is this an EAC Spreadsheet?
    // Also verifies that the Spreadsheet has properties where the script expects.
    function isEACWorkbook(workbook: XLSX.WorkBook) {
        const requiredSheets = [
            "Test Info",
            "Courses Included",
            "Summary Statistics",
            "Item Analysis",
            "Distractors",
            "Student Questions",
            "Student Landscape",
            "Student Coaching",
            "Blueprint"
        ];
        return (
            requiredSheets.every(v => workbook?.Sheets?.hasOwnProperty(v)) &&
            workbook?.Sheets?.["Summary Statistics"]?.A3?.v === "Scorable Questions" &&
            Math.floor(Number(workbook?.Sheets?.["Summary Statistics"]?.B3?.v)) > 0
        );
    }
    console.log(isEACWorkbook(workbook));
    
    const nItems = Math.floor(Number(workbook?.Sheets?.["Summary Statistics"]?.B3?.v));

    const goalsNameSheet = XLSX.utils.sheet_to_json(workbook?.Sheets?.["Goals Summary"]);
    let goals = goalsNameSheet.map((e:any) => {
        return {
            name: String(e?.["Goals"]),
            items: Math.floor(Number(e?.["# Qs"])),
            description: String(e?.["Desc"])
        }
    });
    
    interface JSONRow {
        [index: string]: string | number;
    }
    const itemSheet:JSONRow[]= XLSX.utils.sheet_to_json(workbook?.Sheets?.["Student Questions"]);
    const goalsSheet:JSONRow[] = XLSX.utils.sheet_to_json(workbook?.Sheets?.["Student Goals"]);
    console.log(itemSheet);
    let allStudents = itemSheet
        .filter((v) => String(v?.["Student_id"]).search(/G\d+/i) == 0)
        .map((e) => {
            // for each student
            let responses = [];
            for (const i in e) {
                const iInt = Math.floor(Number(i));
                const v = Math.floor(Number(e?.[i]));
                if (iInt > 0 && v >= 0) responses[iInt-1] = v;
            }

            let goalItems = goals.map((g) => {
                const matchingStudent = goalsSheet.find((i) => {
                    return i?.["Student_id"] === e?.["Student_id"];
                });
                return matchingStudent?.[g.name];
            });
            
            /* THIS SECTION APPLIES ONLY TO THE TEST SPREADSHEET. FINAL DATA WILL BE JOINED FROM SQL. */
            let testSQLData = {
                race: e?.["Race/Ethnicity"],
                gender: e?.["Gender"],
                status: e?.["Status"],
                fg: e?.["First Generation"],
                pell: e?.["Pell"]
            }
            /* END OF TEST SPREADSHEET SECTION */

            return {
                sid: e?.["Student_id"],
                score: responses.reduce((c,p) => c+p),
                responses: responses,
                goals: goalItems,
                ...testSQLData // TEST SPREADSHEET ONLY
            };
        });
    console.log(allStudents);
    console.log(performance.now());
})();

const Report:NextPage = () => {
    return (
        <div>Done. Check log for details.</div>
    )
}

export default Report
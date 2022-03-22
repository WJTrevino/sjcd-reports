FROM node:17

WORKDIR /home/node/app

COPY package*.json ./

RUN npm install fs
RUN npm install
RUN npm ci --only-production
#COPY . .
#RUN npm run build

#EXPOSE 3000
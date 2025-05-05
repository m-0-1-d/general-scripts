# http-file-transfers

## Usage

- Node.js
- Postman
- curl

### Start

Run server:

```
npm run start
```

### Use actions

- upload with data binary:

```
curl -X POST -H "Content-Type: image/jpeg" -H "X-Filename: test1.png" --data-binary "@test1.png" "http://localhost:3033/upload-with-data-binary"
```

- upload with form:

Use Postman and send to the POST http://localhost:3033/upload-with-form form with **image** and **name** fields, where image is .jpg file and name contents text of the name. Send request and find result in the **images** folder.

- upload with base64:

Run script:

```
node ./script/index.js
```
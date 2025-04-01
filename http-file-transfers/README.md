# http-file-transfers

## Usage

### Start

Install libraries:

```
npm install
```

Run server:

```
npm run dev
```

### Use actions

- upload with form:

Use Postman and send to the http://localhost:3033/upload-with-form form with image and name fields, where image is .jpg file and name contents text of the name. Send request and find result in the **images** folder.

- upload with data binary:

```
curl -X POST -H "Content-Type: image/jpeg" -H "X-Filename: test1.jpg" --data-binary "@test1.jpg" "http://localhost:3033/upload-with-data-binary"
```

- upload with base64:

Run script:

```
node ./script/index.js
```
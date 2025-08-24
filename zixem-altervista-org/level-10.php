<?php
$str = "2 UNION SELECT 1, version()";
$uud = convert_uuencode($str);
echo base64_encode($uud);
?>

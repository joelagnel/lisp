


# «.webshell»		(to "webshell")
# «.webshell-old»	(to "webshell-old")
# «.phpnuke»		(to "phpnuke")

# (find-phpdocpage "tutorial")
# (find-phpfuncpage "phpinfo")
# (find-phpfuncpage "require")




#####
#
# ?
# 2004sep24
#
#####

#
# (ee-once (eeb-php))
echo getcwd(), "\n";
#
# (ee-once (eeb-php))
echo system("set"), "\n";
#
# (ee-once (eeb-php))
echo $_SERVER['REMOTE_ADDR'], "\n";
#
# (ee-once (eeb-php))
$ip = $_SERVER['REMOTE_ADDR'];
$goodip = "127.0.0.1";
if ($ip != $goodip) {
  echo "$ip != $goodip\n";
  exit;
}
echo "ok\n";
#
# (ee-once (eeb-php+))
# (find-phpdocpage "index")
phpinfo();

#
# (ee-once (eeb-php))
# (find-phplangpage "types.array")
$arr = array("foo" => "bar", 12 => true);
echo $arr["foo"], "\n"; // bar
echo $arr[12], "\n";    // 1

#





# (find-phpdocpage "index")
# (find-phplangpage "variables.external")
# (find-phpfuncpage "import-request-variables")






#####
#
# webshell
# 2004sep24
#
#####

# «webshell»  (to ".webshell")
#
sudo touch     /var/www/tmp.php
sudo chmod 666 /var/www/tmp.php

#
cat > /var/www/tmp.php <<'%%%'
<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 3.2//EN">
<html>
<head>
<title></title>
</head>
<body>

<?
  $ip = $_SERVER['REMOTE_ADDR'];
  $goodip = "127.0.0.1";
  if ($ip != $goodip) {
    echo "$ip != $goodip\n";
    exit;
  } else {
    $pwd = $_REQUEST['pwd'];
    if (!$pwd) { $pwd = getcwd(); }
    $command = $_REQUEST['command'];
    if ($command) { $result = `cd $pwd\n$command`; }
    ?>

<form action="<? echo basename($_SERVER['SCRIPT_NAME']) ?>" method=get>
at: <input type=text name=pwd value="<? echo $pwd ?>" size=56><br>
do:
<textarea name='command' cols=60 rows=4><? echo $command ?>
</textarea>
<br>
<input type=submit name=do value="Go!">
</form>
<pre>
<? echo $result ?>
</pre>

    <?
  }
?>

</body></html>
%%%

lynx http://127.0.0.1/tmp.php

#
sudo rm /var/www/tmp.php

#





#####
#
# To run shell commands in machines that allow only php
# 2000aug02
#
#####

# «webshell-old»  (to ".webshell-old")
# (find-fline "~/PHP3/tarstuff.php3")
#
cat > /var/www/tmp.php3 <<'---'
<? include ("/home/root/PHP3/functions.php3");
  // error_reporting(1+12+48);
  function v($s) { return htmlspecialchars($s); }
  function p($s) { return htmlspecialchars($s); }
  if (!$pwd)
    $pwd = posix_getcwd();
  echo "<head></head>
    <body>
    <form action=\"http://$HTTP_HOST$SCRIPT_NAME\" method=post>
    cd <input type=text size=60 name=pwd value=\"".v($pwd)."\"><br>
    <input type=text size=63 name=cmd value=\"".v($cmd)."\"><br>
    <input type=submit></form>
    <pre>\n";
  if (!@chdir($pwd)) {
    echo "no such dir: $pwd\n";
  } else {
    if ($cmd) {
      $s = "# $pwd\n# $cmd\n\n";
      $arr = array();
      $cmd = "export EE=" . dirname($SCRIPT_FILENAME) . "/ee.sh; " .
	     "alias ee='. $EE'; " .
             "($cmd) 2>&1";
      exec($cmd, $arr, $exitcode);
      $s .= join("\n", $arr) . "\n";
      if ($exitcode)
        $s .= "\n# exitcode = $exitcode\n";
      echo p($s);
    }
  }
  echo "</pre></body>\n";
?>
---
wget -q -O - 'http://127.0.0.1/tmp.php3'
wget -q -O - 'http://127.0.0.1/tmp.php3?pwd=/home'
wget -q -O - 'http://127.0.0.1/tmp.php3?pwd=/home&cmd=echo+hello'
wget -q -O - 'http://127.0.0.1/tmp.php3?pwd=/home&cmd=ls;false'
wget -q -O - 'http://127.0.0.1/tmp.php3?pwd=/naaa'
lynx http://127.0.0.1/tmp.php3

#




#  Local Variables:
#  coding:               raw-text-unix
#  ee-delimiter-hash:    "\n#\n"
#  ee-anchor-format:     "«%s»"
#  End:

<?php
include('config.php');
include('db.php');
$host  = $_SERVER['HTTP_HOST'];
//$uri   = rtrim(dirname($_SERVER['PHP_SELF']), '/\\');
$userlog=$_COOKIE['sahlicookie']["username"];
$passlog=$_COOKIE['sahlicookie']["passwdord"];
$uri=domainname;
   if (isset($_COOKIE['sahlicookie']))
        {
          if (!(isset($_COOKIE['sahlicookie']["username"])))
            {
              header("Location: http://$host$uri/index.php?note=1");
              exit;
            }
          else
          {
            $selectuser='select * from "Users" where "UsrName"=\''.$_COOKIE['sahlicookie']["username"].'\'';
            $result=ibase_query($connection,$selectuser)or die(ibase_errmsg());
            $row=ibase_fetch_object($result);
            if ($row<>false)
              {
                if (($row->UsrPassword)<>$_COOKIE['sahlicookie']["passwdord"])
                  header("Location: http://$host$uri/index.php?note=2");
              }
            else
                header("Location: http://$host$uri/index.php?note=1");
          }
        }
    else
        {
         header("Location: http://$host$uri/index.php?note=1");
         exit;
        }
 $disconnect=ibase_close($connection);
?>
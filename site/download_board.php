<?php
$hash = $_GET["hash"];
if (preg_match('/^[a-z0-9]{16}$/', $hash)) {
  $subdirectory = substr($hash, 0, 2);
  header('Content-Type: application/json; charset=utf-8');
  header('Content-Encoding: gzip');
  echo file_get_contents("boards/{$subdirectory}/{$hash}.json.gz");
}
?>

<?php
$board = file_get_contents("php://input");
if (json_decode($board) != null) {
	$hash = substr(md5($board), 16);
  $subdirectory = substr($hash, 0, 2);
  $directory = "boards/{$subdirectory}";
  if (!is_dir($directory)) {
    mkdir($directory, 0777, true);
  }
  $fp = gzopen("{$directory}/{$hash}.json.gz", "w9");
  gzwrite($fp, $board);
  gzclose($fp);
	echo $hash;
} else {
	echo "error: invalid board description";
}
?>

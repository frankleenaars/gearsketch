<?php
$board = file_get_contents("php://input");
if (json_decode($board) != null) {
	$hash = substr(md5($board), 16);
	file_put_contents("boards/{$hash}.txt", $board);
	echo $hash;
} else {
	echo "error: invalid board description";
}
?>



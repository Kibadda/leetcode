<?php

require_once "Arrays.php";

$arrays = new Arrays();

echo var_dump($arrays->containsDuplicate([1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1]));
echo var_dump($arrays->isAnagram("tea", "ate"));

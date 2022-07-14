<?php

class Arrays
{
    public function containsDuplicate(array $nums): bool
    {
        return count($nums) != count(array_unique($nums));
    }

    public function isAnagram(string $s, string $t): bool
    {
        $sSplit = str_split($s);
        sort($sSplit);
        $sSorted = implode($sSplit);
        $tSplit = str_split($t);
        sort($tSplit);
        $tSorted = implode($tSplit);

        return $sSorted === $tSorted;
    }
}

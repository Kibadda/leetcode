<?php
class Tree
{
    public function __construct(
        public int $value,
        public ?Tree $left = null,
        public ?Tree $right = null,
    ) {
    }

    public function invert()
    {
        return new Tree($this->value, $this->right?->invert(), $this->left?->invert());
    }
}

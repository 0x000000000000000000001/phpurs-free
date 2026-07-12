<?php

class FreeObj {
    public $tag; // 0 = Pure, 1 = Bind
    public $valueOrFa;
    public $binds;

    public function __construct($tag, $valueOrFa, $binds = null) {
        $this->tag = $tag;
        $this->valueOrFa = $valueOrFa;
        $this->binds = $binds;
    }
}

class BindLeaf {
    public $k;
    public function __construct($k) { $this->k = $k; }
}

class BindNode {
    public $left;
    public $right;
    public function __construct($left, $right) {
        $this->left = $left;
        $this->right = $right;
    }
}

$exports['pureImpl'] = function($a) {
    return new FreeObj(0, $a);
};

$exports['liftF'] = function($fa) {
    return new FreeObj(1, $fa);
};

$_bindImpl = function($free, $k = null) use (&$_bindImpl) {
    if (\func_num_args() < 2) {
        $__args = \func_get_args();
        return function(...$more) use ($__args, &$_bindImpl) {
            return $_bindImpl(...\array_merge($__args, $more));
        };
    }
    
    $newBinds = null;
    if ($free->binds === null) {
        $newBinds = new BindLeaf($k);
    } else {
        $newBinds = new BindNode($free->binds, new BindLeaf($k));
    }
    return new FreeObj($free->tag, $free->valueOrFa, $newBinds);
};

$exports['bindImpl'] = $_bindImpl;

$_resumePrime = function($k, $j = null, $f = null) use (&$_resumePrime) {
    if (\func_num_args() < 3) {
        $__args = \func_get_args();
        return function(...$more) use ($__args, &$_resumePrime) {
            return $_resumePrime(...\array_merge($__args, $more));
        };
    }
    
    while (true) {
        if ($f->tag === 0) { // Pure
            $curr = $f->binds;
            $stack = [];
            $first = null;
            
            while ($curr !== null) {
                if ($curr instanceof BindLeaf) {
                    $first = $curr->k;
                    break;
                } else if ($curr instanceof BindNode) {
                    $stack[] = $curr->right;
                    $curr = $curr->left;
                }
            }

            if ($first === null) {
                return $j($f->valueOrFa);
            }

            $restBinds = null;
            foreach ($stack as $s) {
                if ($restBinds === null) {
                    $restBinds = $s;
                } else {
                    $restBinds = new BindNode($s, $restBinds);
                }
            }

            $f2 = $first($f->valueOrFa);
            
            $newBinds = null;
            if ($f2->binds === null) {
                $newBinds = $restBinds;
            } else if ($restBinds === null) {
                $newBinds = $f2->binds;
            } else {
                $newBinds = new BindNode($f2->binds, $restBinds);
            }
            
            $f = new FreeObj($f2->tag, $f2->valueOrFa, $newBinds);

        } else {
            // Lift
            $cont = function($b) use ($f) {
                return new FreeObj(0, $b, $f->binds);
            };
            $kf = $k($f->valueOrFa);
            return $kf($cont);
        }
    }
};

$exports['resumePrime'] = $_resumePrime;

return $exports;

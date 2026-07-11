<?php

class FreeObj {
    public $tag; // 0 = Pure, 1 = Bind
    public $valueOrFa;
    public $binds;
    public $offset;

    public function __construct($tag, $valueOrFa, array $binds = [], int $offset = 0) {
        $this->tag = $tag;
        $this->valueOrFa = $valueOrFa;
        $this->binds = $binds;
        $this->offset = $offset;
    }
}

$exports['pureImpl'] = function($a) {
    return new FreeObj(0, $a);
};

$exports['liftF'] = function($fa) {
    return new FreeObj(1, $fa);
};

$exports['bindImpl'] = function($free) {
    return function($k) use ($free) {
        $newBinds = $free->binds;
        $newBinds[] = $k;
        return new FreeObj($free->tag, $free->valueOrFa, $newBinds, $free->offset);
    };
};

$exports['resumePrime'] = function($k) {
    return function($j) use ($k) {
        return function($f) use ($k, $j) {
            while (true) {
                if ($f->tag === 0) {
                    if ($f->offset >= count($f->binds)) {
                        $jf = $j($f->valueOrFa);
                        return $jf;
                    }
                    $b = $f->binds[$f->offset];
                    $f2 = $b($f->valueOrFa);
                    
                    $restBinds = array_slice($f->binds, $f->offset + 1);
                    if (empty($restBinds)) {
                        $f = $f2;
                    } else {
                        $f2Binds = array_slice($f2->binds, $f2->offset);
                        $newBinds = array_merge($f2Binds, $restBinds);
                        $f = new FreeObj($f2->tag, $f2->valueOrFa, $newBinds, 0);
                    }
                } else {
                    // Bind
                    $cont = function($b) use ($f) {
                        $restBinds = array_slice($f->binds, $f->offset);
                        return new FreeObj(0, $b, $restBinds, 0);
                    };
                    $kf = $k($f->valueOrFa);
                    return $kf($cont);
                }
            }
        };
    };
};

return $exports;

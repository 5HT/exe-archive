Require Import core.
Require Import homotopy.
Require Import pi.
Require Import stream.
Require Import effect.

Inductive MLTT := Pi:     MLTT   -> MLTT -> MLTT
                | Lam:    Binder -> MLTT -> MLTT
                | App:    MLTT   -> MLTT -> MLTT
                | Sigma:  MLTT   -> MLTT -> MLTT
                | Pair:   MLTT   -> MLTT -> MLTT
                | Fst:    MLTT   -> MLTT
                | Snd:    MLTT   -> MLTT
                | Where:  MLTT   -> Decls -> MLTT
                | Var:    Ident  -> MLTT
                | U:      MLTT
                | Con:    Label  -> list MLTT   -> MLTT
                | Split:  Loc    -> list Branch -> MLTT
                | Sum:    Binder -> NamedSum    -> MLTT
                | HIT:    HomotopyCalculus -> MLTT
                | PI:     PiCalculus       -> MLTT
                | EFF:    EffectCalculus   -> MLTT
                | STREAM: StreamCalculus   -> MLTT.

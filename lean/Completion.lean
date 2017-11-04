/- Completion.lean -/

import Setoid
import Cat
import Functor

namespace EXE

namespace Completion

    record Ob (C : CatType) : Type :=
        (atCat : CatType)
        (atFun : atCat ⟶ C)

    record HomType {C : CatType} (O1 O2 : Ob C) : Type :=
        (onCat : Ob.atCat O2 ⟶ Ob.atCat O1)
        (onFun : (Ob.atFun O1 ⊗ onCat) ⟹ Ob.atFun O2)

    definition HomPreEqu {C : CatType} (O1 O2 : Ob C) : EquType (HomType O1 O2) :=
        λ (m1 m2 : HomType O1 O2),
        ∃ (phi : HomType.onCat m1 ⟹ HomType.onCat m2),
            ((HomType.onFun m2) ⊙(Ob.atCat O2 ⟶ C)⊙ (Ob.atFun O1 ⊗/ phi))
            ≡((Ob.atFun O1 ⊗ HomType.onCat m1) ⟹ Ob.atFun O2)≡
            (HomType.onFun m1)

    definition HomSet (C : CatType) : Cat.HomType (Ob C) :=
        λ (O1 O2 : Ob C), Setoid.Closure (HomPreEqu O1 O2)

end Completion

definition CompletionCat : CatType → CatType :=
    λ (C : CatType),
    Cat.MkOb
        (Completion.Ob C)
        (Completion.HomSet C)
        (sorry)
        (sorry)
        (sorry)
        (sorry)
        (sorry)

end EXE

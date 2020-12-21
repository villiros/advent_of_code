abstract sig Ingredient {count: one CountOfUn}
abstract sig Allergen {contains: one Ingredient}
one sig mxmxvkd extends Ingredient {}
one sig kfcds extends Ingredient {}
one sig sqjhc extends Ingredient {}
one sig nhms extends Ingredient {}
one sig dairy extends Allergen {}
one sig fish extends Allergen {}

fact Rule_1 {
    one (dairy.contains & (mxmxvkd + kfcds + sqjhc + nhms  ))
    one (fish.contains & (mxmxvkd + kfcds + sqjhc + nhms  ))
}
one sig trh extends Ingredient {}
one sig fvjkl extends Ingredient {}
one sig sbzzf extends Ingredient {}

fact Rule_2 {
    one (dairy.contains & (trh + fvjkl + sbzzf + mxmxvkd  ))
}
one sig soy extends Allergen {}

fact Rule_3 {
    one (soy.contains & (sqjhc + fvjkl  ))
}

fact Rule_4 {
    one (fish.contains & (sqjhc + mxmxvkd + sbzzf  ))
}
abstract sig CountOfUn{}
one sig  Count_sbzzf_2  extends CountOfUn {}
one sig  Count_kfcds_1  extends CountOfUn {}
one sig  Count_fvjkl_2  extends CountOfUn {}
one sig  Count_nhms_1  extends CountOfUn {}
one sig  Count_trh_1  extends CountOfUn {}
one sig  Count_sqjhc_3  extends CountOfUn {}
one sig  Count_mxmxvkd_3  extends CountOfUn {}

fact IngCounts {
    sbzzf.count = Count_sbzzf_2
    kfcds.count = Count_kfcds_1
    fvjkl.count = Count_fvjkl_2
    nhms.count = Count_nhms_1
    trh.count = Count_trh_1
    sqjhc.count = Count_sqjhc_3
    mxmxvkd.count = Count_mxmxvkd_3
}
fact OneAlergenPerIng {
    all a, b: Allergen | a != b => no (a.contains & b.contains)
}
one sig NoAllerg {
    contains: set Ingredient,
    count_appear: set CountOfUn
}
fact NoAllergSet {
    NoAllerg.contains = Ingredient - Allergen.contains
    NoAllerg.count_appear = NoAllerg.contains.count
}

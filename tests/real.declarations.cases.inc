{ Complete original expressions test declarations, value conversions and
  distinct target identities alongside runtime/typed-constant proof barriers. }
Emit('name',True,Spacing);
Emit('alias',True,AliasValue);
Emit('third',True,Third);
Emit('named-use',True,Spacing + 2);
Emit('named-integer',True,Seed * Spacing);
Emit('named-negative',True,-Spacing);
Emit('named-cast',True,Single(AliasValue));
Emit('named-rounded',True,RoundedSingle);
Emit('cast-single',True,Single(16777217));
Emit('cast-double',True,Double(9007199254740993));
Emit('cast-extended',True,Extended(18446744073709551615));
Emit('cast-negative',True,Single(-(1.25)));
Emit('cast-decimal',True,Single(0.1));
Emit('cast-nested',True,Double(Single(0.1)));
Emit('cast-sum',True,Single(0.1) + Single(0.2));
Emit('cast-distinct',True,TAmount(1.25));
Emit('cast-runtime',False,Single(F64));
Emit('typed-constant',False,TypedSpacing);
Emit('typed-negative',False,-TypedSpacing);
Emit('typed-cast',False,Double(TypedSpacing));

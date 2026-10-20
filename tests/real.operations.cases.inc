{ The same complete expressions are compiled at five authored switch sites.
  Metadata spells their operands independently of the compiler expression.
  Operation numbers are witness data: arithmetic 0..3, relations 4..9. }
Emit('add','1.25','2.5',0,1.25 + 2.5);
Emit('subtract','1.25','2.5',1,1.25 - 2.5);
Emit('multiply','1.25','2.5',2,1.25 * 2.5);
Emit('tenth','1.0','10.0',3,1.0 / 10.0);
Emit('third','1.0','3.0',3,1.0 / 3.0);
Emit('single-boundary','16777216.0','1.0',0,16777216.0 + 1.0);
Emit('double-boundary','9007199254740992.0','1.0',0,9007199254740992.0 + 1.0);
Emit('decimal-sum','0.1','0.2',0,0.1 + 0.2);
Emit('near-single-half','1.000000059604644775390625','1.0e-30',0,
  1.000000059604644775390625 + 1.0e-30);
Emit('negative-zero-add','-0.0','-0.0',0,-0.0 + (-0.0));
Emit('negative-zero-subtract','-0.0','-0.0',1,-0.0 - (-0.0));
Emit('negative-zero-multiply','-0.0','2.0',2,-0.0 * 2.0);
Emit('negative-zero-divide','-0.0','-2.0',3,-0.0 / (-2.0));
Emit('cancellation','1.0e300','1.0e300',1,1.0e300 - 1.0e300);
Emit('large-product','1.0e300','1.0e-300',2,1.0e300 * 1.0e-300);
Emit('large-quotient','1.0e300','1.0e300',3,1.0e300 / 1.0e300);
Emit('subnormal','1.0e-4932','2.0',3,1.0e-4932 / 2.0);
EmitRelation('equal','-0.0','0.0',4,-0.0 = 0.0);
EmitRelation('not-equal','0.1','0.2',5,0.1 <> 0.2);
EmitRelation('less','-2.5','1.25',6,-2.5 < 1.25);
EmitRelation('less-equal','0.1','0.1',7,0.1 <= 0.1);
EmitRelation('greater','2.5','1.25',8,2.5 > 1.25);
EmitRelation('greater-equal','-2.5','-1.25',9,-2.5 >= -1.25);

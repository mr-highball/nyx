{ Repeated unchanged at independently compiled H settings. The proof flag
  identifies constant expressions, never a predicted native result type. }
Emit('literal-pair', True, 'ab' + 'cd');
Emit('literal-characters', True, 'a' + 'b');
Emit('literal-empty', True, '' + '');
Emit('literal-one', True, 'a' + '');
Emit('literal-nul', True, 'a'#0 + 'b');
Emit('literal-wide', True, 'a' + #256);
Emit('literal-wide-pair', True, #256 + #257);
Emit('literal-byte-wide', True, #233 + #256);
Emit('literal-utf8-wide', True, #195#169 + #256);
Emit('literal-surrogate', True, #$D800 + 'x');
Emit('named-caption', True, Caption + '!');
Emit('named-characters', True, Letter + Letter);
Emit('named-wide', True, WideLetter + 'x');
Emit('named-equal', True, Caption = 'Hello, world');
Emit('literal-less', True, 'a' < 'aa');
Emit('literal-nul-less', True, 'a'#0 < 'a'#1);
Emit('literal-surrogate-less', True, #$D800 < #$D801);
Emit('runtime-ansi', False, A + '!');
Emit('runtime-short', False, S + S);
Emit('runtime-ansi-short', False, A + S);
Emit('runtime-utf8', False, U + A);
Emit('runtime-unicode', False, D + A);
Emit('runtime-wide', False, W + A);
Emit('runtime-unicode-wide', False, D + W);
Emit('runtime-characters', False, C + C);
Emit('runtime-wide-character', False, WC + C);
Emit('runtime-literal-short', False, 'Hello' + S);
Emit('runtime-short-literal', False, S + '!');
Emit('runtime-character-literal', False, C + '!');
Emit('runtime-distinct', False, Dist + '!');
Emit('runtime-equal', False, A = 'Hello');
Emit('generic-return', False, Keep('ab' + 'cd'));
Emit('typed-storage', False, TypedCaption + '!');
Emit('bounded-short', False, Small + '!');
Emit('named-prefix', True, Prefix + '!');
Emit('named-prefix-generic', False, Keep(Prefix));
Emit('character-chain', True, ('a' + 'b') + 'c');
Emit('string-chain', True, ('ab' + 'c') + 'd');
Emit('empty-leading', True, '' + 'a');
Emit('byte-chain-wide', True, (#195 + #169) + #256);
Emit('runtime-widechar-pair', False, WC + WC);
Emit('runtime-wide-string-character', False, W + WC);
Emit('runtime-character-wide', False, C + WC);
Emit('runtime-widechar-short', False, WC + S);
Emit('runtime-widechar-wide', False, WC + W);
Emit('runtime-short-widechar', False, S + WC);
{$push}{$H+}
Emit('pending-h-before-left', True, {$H-}'ab' + 'cd');
{$H+}
Emit('pending-h-before-right', True, 'ab' + {$H-}'cd');
{$H+}
Emit('pending-h-after-right', True, 'ab' + 'cd' {$H-});
{$H+}
Emit('flush-h-after-right', True, 'ab' + 'cd' {$H-}{$ifopt H+}{$endif});
{$H+}
Emit('reference-pending-h', False, Keep(Prefix {$H-}));
{$H+}
Emit('reference-leading-h', False, Keep({$H-}Prefix));
{$H+}
Emit('literal-generic-pending-h', False, Keep('ab' {$H-}));
{$H+}
Emit('folded-generic-pending-h', False, Keep('ab' + 'cd' {$H-}));
{$pop}

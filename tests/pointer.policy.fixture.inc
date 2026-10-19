{ Nested requests must preserve their own include and settings-stack origin. }
{$push}
{$checkpointer-}
const
  PointerIncludedOff = 4;
{$checkpointer*}
const
  PointerIncludedDefault = 5;
{$pop}
const
  PointerAfterInclude = 6;

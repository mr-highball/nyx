{ nyx
  Copyright (c) 2020 mr-highball

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
}

{ Declarative expression parsing invokes the public typed constructors and
  methods. It builds immutable descriptors only; runtime state is never evaluated
  while reading source, and arbitrary Pascal remains the compiler's responsibility. }

function TConfigurationReader.ComputedExpressionConstructor(const AName: TNyxText): TValue;
var
  LArgs: TValues;
  LCurrent: INyxExpression;
  LRight: INyxExpression;
  LThen: INyxExpression;
  LElse: INyxExpression;
  LCondition: INyxExpression;
  LFamily: TNyxStateKind;
  LInteger: Integer;
  LMethod: TNyxText;
  LComparison: TNyxExpressionTextComparison;

  function Operand(const AValue: TValue): INyxExpression;
  begin

    if AValue.Kind <> vkExpression then
    begin
      Fail('Computed operator requires its typed expression operand');
    end;
    Result := NyxExpressionFromData(AValue.ExpressionData);
  end;

  procedure SameFamily;
  begin

    if LCurrent.Kind <> LRight.Kind then
    begin
      Fail('Computed operator operands have different scalar families');
    end;
  end;

begin
  LArgs := Arguments;

  if SameText(AName, 'NyxChoose') then
  begin

    if Length(LArgs) <> 3 then
    begin
      Fail('NyxChoose requires its Boolean condition and two typed branches');
    end;
    LCondition := Operand(LArgs[0]);
    LThen := Operand(LArgs[1]);
    LElse := Operand(LArgs[2]);

    if (LCondition.Kind <> nskBoolean) or (LThen.Kind <> LElse.Kind) then
    begin
      Fail('NyxChoose requires Boolean condition and identical branch families');
    end;
    case LThen.Kind of
      nskBoolean:
        begin
          LCurrent := NyxChoose(LCondition as INyxBooleanExpression,
            LThen as INyxBooleanExpression, LElse as INyxBooleanExpression);
        end;
      nskText:
        begin
          LCurrent := NyxChoose(LCondition as INyxBooleanExpression,
            LThen as INyxTextExpression, LElse as INyxTextExpression);
        end;
      nskInteger:
        begin
          LCurrent := NyxChoose(LCondition as INyxBooleanExpression,
            LThen as INyxIntegerExpression, LElse as INyxIntegerExpression);
        end;
      nskNumber:
        begin
          LCurrent := NyxChoose(LCondition as INyxBooleanExpression,
            LThen as INyxNumberExpression, LElse as INyxNumberExpression);
        end;
    end;
  end
  else
  begin

    if Length(LArgs) <> 1 then
    begin
      Fail('Expression factory requires one typed literal or state reference');
    end;
    LFamily := nskText;

    if SameText(AName, 'NyxBooleanExpression') then
    begin
      LFamily := nskBoolean;
    end
    else if SameText(AName, 'NyxIntegerExpression') then
    begin
      LFamily := nskInteger;
    end
    else if SameText(AName, 'NyxNumberExpression') then
    begin
      LFamily := nskNumber;
    end;

    if LArgs[0].Kind = vkStateRef then
    begin

      if LArgs[0].Ordinal <> Ord(LFamily) then
      begin
        Fail('Expression factory requires its distinct typed state reference');
      end;
      case LFamily of
        nskBoolean:
          begin
            LCurrent := NyxBooleanExpression(NyxBooleanState(LArgs[0].Text));
          end;
        nskText:
          begin
            LCurrent := NyxTextExpression(NyxTextState(LArgs[0].Text));
          end;
        nskInteger:
          begin
            LCurrent := NyxIntegerExpression(NyxIntegerState(LArgs[0].Text));
          end;
        nskNumber:
          begin
            LCurrent := NyxNumberExpression(NyxNumberState(LArgs[0].Text));
          end;
      end;
    end
    else
    begin
      case LFamily of
        nskText:
          begin

            if LArgs[0].Kind <> vkText then
            begin
              Fail('Text expression requires a text literal');
            end;
            LCurrent := NyxTextExpression(LArgs[0].Text);
          end;
        nskBoolean:
          begin

            if LArgs[0].Kind <> vkBoolean then
            begin
              Fail('Boolean expression requires a Boolean literal');
            end;
            LCurrent := NyxBooleanExpression(LArgs[0].Ordinal <> 0);
          end;
        nskInteger:
          begin

            if (LArgs[0].Kind <> vkInteger) or not TryNyxStateInteger(LArgs[0].Text, LInteger) then
            begin
              Fail('Integer expression requires a signed Integer literal');
            end;
            LCurrent := NyxIntegerExpression(LInteger);
          end;
        nskNumber:
          begin

            if not (LArgs[0].Kind in [vkInteger, vkNumber]) then
            begin
              Fail('Number expression requires an explicit numeric literal');
            end;
            LCurrent := NyxNumberExpression(Numeric(LArgs[0]));
          end;
      end;
    end;
  end;
  while At('.') do
  begin
    Expect('.');

    if (FCursor >= Length(FTokens)) or (FTokens[FCursor].Kind <> tkWord) then
    begin
      Fail('Expected a typed expression method');
    end;
    LMethod := LowerCase(FTokens[FCursor].Text);
    Inc(FCursor);
    LArgs := nil;

    if At('(') then
    begin
      LArgs := Arguments;
    end;

    if Length(LArgs) = 0 then
    begin
      case LCurrent.Kind of
        nskBoolean:
          begin
            if LMethod = 'negated' then
            begin
              LCurrent := (LCurrent as INyxBooleanExpression).Negated;
            end
            else if LMethod = 'astext' then
            begin
              LCurrent := (LCurrent as INyxBooleanExpression).AsText;
            end
            else
            begin
              Fail('Unsupported unary boolean expression method');
            end;
          end;
        nskText:
          begin
            if LMethod = 'length' then
            begin
              LCurrent := (LCurrent as INyxTextExpression).Length;
            end
            else
            begin
              Fail('Unsupported unary text expression method');
            end;
          end;
        nskInteger:
          begin
            if LMethod = 'negated' then
            begin
              LCurrent := (LCurrent as INyxIntegerExpression).Negated;
            end
            else if LMethod = 'astext' then
            begin
              LCurrent := (LCurrent as INyxIntegerExpression).AsText;
            end
            else if LMethod = 'asnumber' then
            begin
              LCurrent := (LCurrent as INyxIntegerExpression).AsNumber;
            end
            else
            begin
              Fail('Unsupported unary integer expression method');
            end;
          end;
        nskNumber:
          begin
            if LMethod = 'negated' then
            begin
              LCurrent := (LCurrent as INyxNumberExpression).Negated;
            end
            else if LMethod = 'astext' then
            begin
              LCurrent := (LCurrent as INyxNumberExpression).AsText;
            end
            else
            begin
              Fail('Unsupported unary number expression method');
            end;
          end;
      end;
      Continue;
    end;

    if (Length(LArgs) < 1) or (Length(LArgs) > 2) then
    begin
      Fail('Computed binary operator requires one typed expression operand');
    end;
    LRight := Operand(LArgs[0]);
    SameFamily;
    LComparison := netExact;

    if Length(LArgs) = 2 then
    begin

      if (LCurrent.Kind <> nskText) or (LArgs[1].Kind <> vkExpressionTextComparison) then
      begin
        Fail('Only text predicates accept a typed comparison mode');
      end;
      LComparison := TNyxExpressionTextComparison(LArgs[1].Ordinal);

      if LMethod = 'append' then
      begin
        Fail('Append has no text comparison argument');
      end;
    end;
    case LCurrent.Kind of
      nskBoolean:
        begin
          if LMethod = 'andalso' then
          begin
            LCurrent := (LCurrent as INyxBooleanExpression).AndAlso(
              LRight as INyxBooleanExpression);
          end
          else if LMethod = 'orelse' then
          begin
            LCurrent := (LCurrent as INyxBooleanExpression).OrElse(
              LRight as INyxBooleanExpression);
          end
          else if LMethod = 'equalto' then
          begin
            LCurrent := (LCurrent as INyxBooleanExpression).EqualTo(
              LRight as INyxBooleanExpression);
          end
          else if LMethod = 'notequalto' then
          begin
            LCurrent := (LCurrent as INyxBooleanExpression).NotEqualTo(
              LRight as INyxBooleanExpression);
          end
          else
          begin
            Fail('Unsupported binary boolean expression method');
          end;
        end;
      nskText:
        begin
          if LMethod = 'append' then
          begin
            LCurrent := (LCurrent as INyxTextExpression).Append(
              LRight as INyxTextExpression);
          end
          else if LMethod = 'equalto' then
          begin
            LCurrent := (LCurrent as INyxTextExpression).EqualTo(
              LRight as INyxTextExpression, LComparison);
          end
          else if LMethod = 'notequalto' then
          begin
            LCurrent := (LCurrent as INyxTextExpression).NotEqualTo(
              LRight as INyxTextExpression, LComparison);
          end
          else if LMethod = 'contains' then
          begin
            LCurrent := (LCurrent as INyxTextExpression).Contains(
              LRight as INyxTextExpression, LComparison);
          end
          else if LMethod = 'startswith' then
          begin
            LCurrent := (LCurrent as INyxTextExpression).StartsWith(
              LRight as INyxTextExpression, LComparison);
          end
          else if LMethod = 'endswith' then
          begin
            LCurrent := (LCurrent as INyxTextExpression).EndsWith(
              LRight as INyxTextExpression, LComparison);
          end
          else
          begin
            Fail('Unsupported binary text expression method');
          end;
        end;
      nskInteger:
        begin
          if LMethod = 'plus' then
          begin
            LCurrent := (LCurrent as INyxIntegerExpression).Plus(
              LRight as INyxIntegerExpression);
          end
          else if LMethod = 'minus' then
          begin
            LCurrent := (LCurrent as INyxIntegerExpression).Minus(
              LRight as INyxIntegerExpression);
          end
          else if LMethod = 'times' then
          begin
            LCurrent := (LCurrent as INyxIntegerExpression).Times(
              LRight as INyxIntegerExpression);
          end
          else if LMethod = 'dividedby' then
          begin
            LCurrent := (LCurrent as INyxIntegerExpression).DividedBy(
              LRight as INyxIntegerExpression);
          end
          else if LMethod = 'modulo' then
          begin
            LCurrent := (LCurrent as INyxIntegerExpression).Modulo(
              LRight as INyxIntegerExpression);
          end
          else if LMethod = 'equalto' then
          begin
            LCurrent := (LCurrent as INyxIntegerExpression).EqualTo(
              LRight as INyxIntegerExpression);
          end
          else if LMethod = 'notequalto' then
          begin
            LCurrent := (LCurrent as INyxIntegerExpression).NotEqualTo(
              LRight as INyxIntegerExpression);
          end
          else if LMethod = 'lessthan' then
          begin
            LCurrent := (LCurrent as INyxIntegerExpression).LessThan(
              LRight as INyxIntegerExpression);
          end
          else if LMethod = 'atmost' then
          begin
            LCurrent := (LCurrent as INyxIntegerExpression).AtMost(
              LRight as INyxIntegerExpression);
          end
          else if LMethod = 'greaterthan' then
          begin
            LCurrent := (LCurrent as INyxIntegerExpression).GreaterThan(
              LRight as INyxIntegerExpression);
          end
          else if LMethod = 'atleast' then
          begin
            LCurrent := (LCurrent as INyxIntegerExpression).AtLeast(
              LRight as INyxIntegerExpression);
          end
          else
          begin
            Fail('Unsupported binary integer expression method');
          end;
        end;
      nskNumber:
        begin
          if LMethod = 'plus' then
          begin
            LCurrent := (LCurrent as INyxNumberExpression).Plus(
              LRight as INyxNumberExpression);
          end
          else if LMethod = 'minus' then
          begin
            LCurrent := (LCurrent as INyxNumberExpression).Minus(
              LRight as INyxNumberExpression);
          end
          else if LMethod = 'times' then
          begin
            LCurrent := (LCurrent as INyxNumberExpression).Times(
              LRight as INyxNumberExpression);
          end
          else if LMethod = 'dividedby' then
          begin
            LCurrent := (LCurrent as INyxNumberExpression).DividedBy(
              LRight as INyxNumberExpression);
          end
          else if LMethod = 'equalto' then
          begin
            LCurrent := (LCurrent as INyxNumberExpression).EqualTo(
              LRight as INyxNumberExpression);
          end
          else if LMethod = 'notequalto' then
          begin
            LCurrent := (LCurrent as INyxNumberExpression).NotEqualTo(
              LRight as INyxNumberExpression);
          end
          else if LMethod = 'lessthan' then
          begin
            LCurrent := (LCurrent as INyxNumberExpression).LessThan(
              LRight as INyxNumberExpression);
          end
          else if LMethod = 'atmost' then
          begin
            LCurrent := (LCurrent as INyxNumberExpression).AtMost(
              LRight as INyxNumberExpression);
          end
          else if LMethod = 'greaterthan' then
          begin
            LCurrent := (LCurrent as INyxNumberExpression).GreaterThan(
              LRight as INyxNumberExpression);
          end
          else if LMethod = 'atleast' then
          begin
            LCurrent := (LCurrent as INyxNumberExpression).AtLeast(
              LRight as INyxNumberExpression);
          end
          else
          begin
            Fail('Unsupported binary number expression method');
          end;
        end;
    end;
  end;
  Result := Default(TValue);
  Result.Kind := vkExpression;
  Result.Ordinal := Ord(LCurrent.Kind);
  Result.ExpressionData := LCurrent.ToData;
end;

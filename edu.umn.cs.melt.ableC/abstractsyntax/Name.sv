grammar edu:umn:cs:melt:ableC:abstractsyntax;

synthesized attribute name :: String;

synthesized attribute valueRedeclarationCheck :: [Message];
synthesized attribute labelRedeclarationCheck :: [Message];
synthesized attribute tagRedeclarationCheck :: [Message];

synthesized attribute valueLookupCheck :: [Message];
synthesized attribute labelLookupCheck :: [Message];
synthesized attribute tagLookupCheck :: [Message];

synthesized attribute valueItem :: Decorated ValueItem;
synthesized attribute labelItem :: Decorated LabelItem;
synthesized attribute tagItem :: Decorated TagItem;


nonterminal Name with location, name, pp, env, valueRedeclarationCheck, labelRedeclarationCheck, valueLookupCheck, labelLookupCheck, tagLookupCheck, valueItem, labelItem, tagItem;

abstract production name
top::Name ::= n::String
{
  top.name = n;
  top.pp = text(n);
  
  local valdcls :: [ValueItem] = lookupValueInLocalScope(n, top.env);
  top.valueRedeclarationCheck =
    case valdcls of
    | [] -> []
    | v :: _ -> [err(top.location, "Redeclaration of " ++ n)]
    end;
  
  local labdcls :: [LabelItem] = lookupLabelInLocalScope(n, top.env);
  top.labelRedeclarationCheck =
    case labdcls of
    | [] -> error("Internal error: expected to find label in function scope, was missing.")
    | _ :: [] -> [] -- We found ourselves. Labels are in function scope, so a-okay!
    | _ :: _ :: _ -> [err(top.location, "Redeclaration of " ++ n)]
    end;
  
  local values :: [ValueItem] = lookupValue(n, top.env);
  local tags :: [TagItem] = lookupTag(n, top.env);
  local labels :: [LabelItem] = lookupLabel(n, top.env);
  top.valueLookupCheck =
    case values of
    | [] -> [err(top.location, "Undeclared " ++ n)]
    | _ :: _ -> []
    end;
  top.labelLookupCheck =
    case labels of
    | [] -> [err(top.location, "Undeclared " ++ n)]
    | _ :: _ -> []
    end;
  top.tagLookupCheck =
    case tags of
    | [] -> [err(top.location, "Undeclared " ++ n)]
    | _ :: _ -> []
    end;
  
  local value :: ValueItem = if null(values) then errorValueItem() else head(values);
  local tag :: TagItem = if null(tags) then errorTagItem() else head(tags);
  local label :: LabelItem = if null(labels) then errorLabelItem() else head(labels);
  
  top.valueItem = value;
  top.tagItem = tag;
  top.labelItem = label;
}

synthesized attribute maybename :: Maybe<Name>;

nonterminal MaybeName with maybename, pp, env, tagRedeclarationCheck, valueRedeclarationCheck;

abstract production justName
top::MaybeName ::= n::Name
{
  top.pp = n.pp;
  top.maybename = just(n);

  top.valueRedeclarationCheck = n.valueRedeclarationCheck;

  local tagdcls :: [TagItem] = lookupTagInLocalScope(n.name, top.env);
  top.tagRedeclarationCheck =
    case tagdcls of
    | [] -> []
    | v :: _ -> [err(n.location, "Redeclaration of " ++ n.name)]
    end;
  
}
abstract production nothingName
top::MaybeName ::=
{
  top.pp = notext();
  top.maybename = nothing();

  top.valueRedeclarationCheck = [];
  top.tagRedeclarationCheck = [];
}


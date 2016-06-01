module Tehanu.FSharp.Quote

open System

open Tehanu.Core            
open Tehanu.Core.Generators
open Tehanu.Core.Patterns
open Tehanu.Attributes.Parser

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns
open Microsoft.FSharp.Collections
      
let parseAttr (attr: Tree) =
  match attr with
  | Attr (name, ExprConstString txt) ->                
    genAttr name (parseAttribute txt)
  | _ -> Atom ("not supported attr " + string attr)

let rec createExpr (expr: Expr): Tree =
  match expr with
  | Var v -> genExprId v.Name
  | IfThenElse(c, t, e) -> genExprIf (createExpr c) (createExpr t) (createExpr e)
  | Int32(i) -> genExprConstInt i
  | Unit -> genExprConstUnit ()
  | AndAlso(e1, e2) -> genExprApp (genExprApp (genExprId "&&") (createExpr e1)) (createExpr e2)
  | OrElse(e1, e2) -> genExprApp (genExprApp (genExprId "||") (createExpr e1)) (createExpr e2)
  | Application(l, r) -> genExprApp (createExpr l) (createExpr r)                                                                                  
  | SpecificCall <@@ (+) @@> (_, _, exprList) -> genExprApp (genExprApp (genExprId "+") (createExpr exprList.Head)) (createExpr exprList.Tail.Head)
  | SpecificCall <@@ (-) @@> (_, _, exprList) -> genExprApp (genExprApp (genExprId "-") (createExpr exprList.Head)) (createExpr exprList.Tail.Head)
  | SpecificCall <@@ (*) @@> (_, _, exprList) -> genExprApp (genExprApp (genExprId "*") (createExpr exprList.Head)) (createExpr exprList.Tail.Head)
  | SpecificCall <@@ (/) @@> (_, _, exprList) -> genExprApp (genExprApp (genExprId "/") (createExpr exprList.Head)) (createExpr exprList.Tail.Head)
  | SpecificCall <@@ (>) @@> (_, _, exprList) -> genExprApp (genExprApp (genExprId ">") (createExpr exprList.Head)) (createExpr exprList.Tail.Head)
  | SpecificCall <@@ (<) @@> (_, _, exprList) -> genExprApp (genExprApp (genExprId "<") (createExpr exprList.Head)) (createExpr exprList.Tail.Head)
  | SpecificCall <@@ (>=) @@> (_, _, exprList) -> genExprApp (genExprApp (genExprId ">=") (createExpr exprList.Head)) (createExpr exprList.Tail.Head)
  | SpecificCall <@@ (<=) @@> (_, _, exprList) -> genExprApp (genExprApp (genExprId "<=") (createExpr exprList.Head)) (createExpr exprList.Tail.Head)
  | _ -> Atom <| "not supported expr " + string expr

let createTotal (expr: string): Tree =
  genList [parseAttr (genAttr "Total" (genExprConstString expr))]

let createType (typ: Type): Tree =
  if typ.Equals typeof<int> then genTypeId "int" else Atom <| "not supported type" + string typ

let createArgss (argss: list<list<Var>>): Tree =
  genList [for args in argss -> genList [for arg in args -> genArg (arg.Name) (createType arg.Type)]]

let createLet (name: string) (totalGuard: string) (quote: Expr): Tree =
  match quote with
  | Lambdas (argss, expr) ->
    genLet (createTotal totalGuard) name (Pair(ref <| createArgss argss, ref <| genArg "_" (genTypeId "int"))) (createExpr expr)
  | _ -> Atom <| "not supported function body " + string quote

let createModule (name: string) (lets: list<string * string * Expr>): Tree =
  genModule name (genList [for (n, g, q) in lets -> createLet n g q])

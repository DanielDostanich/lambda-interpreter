module Main where

import Lib

main :: IO ()
main = do
    print $ let env = extendEnv (Env []) "x" (TVar "a" :-> TVar "b") in freeTVarsEnv env
    -- ["a","b"]


    let tx = (TVar "a" :-> TVar "b") :-> TVar "c"
    let ty = TVar "a" :-> TVar "b"
    let env = Env [("y",ty),("x",tx)]
    print $ let Right res = appEnv env "x" in res
    -- (TVar "a" :-> TVar "b") :-> TVar "c"
    print $ let Right res = appEnv env "y" in res
    -- TVar "a" :-> TVar "b"
    print $ let Left res = appEnv env "z" in res
    -- "There is no variable \"z\" in the enviroment."


    print $ let sbs = SubsTy [("a", TVar "b")] in appSubsTy sbs (TVar "a" :-> TVar "a")
    -- TVar "b" :-> TVar "b"


    print $ SubsTy [("a", TVar "b")] `mappend` SubsTy [("b", TVar "a")]
    -- SubsTy [("a",TVar "b"),("b",TVar "b")]


    print $ let Right sbs = unify (TVar "a" :-> TVar "b") (TVar "c") in sbs
    --SubsTy [("c",TVar "a" :-> TVar "b")]
    print $ let Right sbs = unify (TVar "a" :-> TVar "b") (TVar "c" :-> TVar "d") in sbs
    -- SubsTy [("a",TVar "c"),("b",TVar "d")]
    print $ let Left tst = unify (TVar "a") (TVar "a" :-> TVar "a") in tst
    -- "Can't unify (TVar \"a\") with (TVar \"a\" :-> TVar \"a\")!"

    let term = Lam "y" $ Var "x"
    let env = Env [("x",TVar "a" :-> TVar "b")]
    print $ let Right eqs = equations env term (TVar "o") in eqs
    -- [(TVar "d",TVar "a" :-> TVar "b"),(TVar "c" :-> TVar "d",TVar "o")]
    print $ let Left err = equations (Env []) term (TVar "o") in err
    -- "There is no variable \"x\" in the enviroment."


    print $ let Right pp = principlePair (Var "x") in pp
    -- (Env [("x",TVar "a")],TVar "a")
    print $ let Right pp = principlePair (Var "f" :@ Var "x") in pp
    -- (Env [("f",TVar "a" :-> TVar "b"),("x",TVar "a")],TVar "b")
    print $ let Right pp = principlePair (Lam "x" $ Lam "y" $ Var "y") in pp
    -- (Env [],TVar "a" :-> (TVar "b" :-> TVar "b"))
    print $ let Left err = principlePair (Var "x" :@ Var "x") in err
    -- "Can't unify (TVar \"a\") with (TVar \"a\" :-> TVar \"b\")!"
    
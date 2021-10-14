# lambda-interpreter

Проект реализует набор функций для работы с просто типизированным лямбда-исчислением

Используемые типы: 

    -- Терм
    data Expr = Var Symb 
            | Expr :@ Expr
            | Lam Symb Expr
    deriving (Eq,Show)

    -- Тип
    data Type = TVar Symb 
            | Type :-> Type
    deriving (Eq,Show)

    -- Контекст
    newtype Env = Env [(Symb,Type)]
    deriving (Eq,Show)

    -- Подстановка
    newtype SubsTy = SubsTy [(Symb, Type)]
    deriving (Eq,Show)

Реализованные функции: 

Возвращает список свободных переменных терма

    freeVars :: Expr -> [Symb] 

Возвращает список свободных переменных типа

    freeTVars :: Type -> [Symb]

Расширяет контекст переменной с заданным типом 

    extendEnv :: Env -> Symb -> Type -> Env

Возвращает список свободных типовых переменных контекста

    freeTVarsEnv :: Env -> [Symb]

    let env = extendEnv (Env []) "x" (TVar "a" :-> TVar "b") in freeTVarsEnv env
    -- ["a","b"]

Позволяет использовать контекст как частичную функцию из переменных в типы

    appEnv :: MonadError String m => Env -> Symb -> m Type

    tx = (TVar "a" :-> TVar "b") :-> TVar "c"
    ty = TVar "a" :-> TVar "b"
    env = Env [("y",ty),("x",tx)]
    let Right res = appEnv env "x" in res
    -- (TVar "a" :-> TVar "b") :-> TVar "c"
    let Right res = appEnv env "y" in res
    -- TVar "a" :-> TVar "b"
    let Left res = appEnv env "z" in res
    -- "There is no variable \"z\" in the enviroment."

Реализует подстановку типов вместо переменных типа

    appSubsTy :: SubsTy -> Type -> Type

Реализует подстановку типов вместо переменных типа в контекст

    appSubsEnv :: SubsTy -> Env -> Env

    let sbs = SubsTy [("a", TVar "b")] in appSubsTy sbs (TVar "a" :-> TVar "a")
    -- TVar "b" :-> TVar "b"

Реализует композицию двух подстановок (носитель композиции является объединением носителей двух этих подстановок)

    composeSubsTy :: SubsTy -> SubsTy -> SubsTy

    SubsTy [("a", TVar "b")] `mappend` SubsTy [("b", TVar "a")]
    -- SubsTy [("a",TVar "b"),("b",TVar "b")]

Реализует механизм унификации: возвращает для двух перестановок наиболее общий унификатор или сообщение об ошибке, если унификация невозможна

    unify :: MonadError String m => Type -> Type -> m SubsTy

    let Right sbs = unify (TVar "a" :-> TVar "b") (TVar "c") in sbs
    --SubsTy [("c",TVar "a" :-> TVar "b")]
    let Right sbs = unify (TVar "a" :-> TVar "b") (TVar "c" :-> TVar "d") in sbs
    -- SubsTy [("a",TVar "c"),("b",TVar "d")]
    let Left tst = unify (TVar "a") (TVar "a" :-> TVar "a") in tst
    -- "Can't unify (TVar \"a\") with (TVar \"a\" :-> TVar \"a\")!"

Реализует алгоритм построения системы уравнений на типы для заданных контекста, терма и инициализирующего типа для терма 

    equations :: MonadError String m => Env -> Expr -> Type -> m [(Type,Type)]

    term = Lam "y" $ Var "x"
    env = Env [("x",TVar "a" :-> TVar "b")]
    let Right eqs = equations env term (TVar "o") in eqs
    -- [(TVar "d",TVar "a" :-> TVar "b"),(TVar "c" :-> TVar "d",TVar "o")]
    let Left err = equations (Env []) term (TVar "o") in err
    -- "There is no variable \"x\" in the enviroment."

Реализует алгоритм поиска главной пары для терма бестипового лямбда-исчисления

    principlePair :: MonadError String m => Expr -> m (Env,Type)

    let Right pp = principlePair (Var "x") in pp
    -- (Env [("x",TVar "a")],TVar "a")
    let Right pp = principlePair (Var "f" :@ Var "x") in pp
    -- (Env [("f",TVar "a" :-> TVar "b"),("x",TVar "a")],TVar "b")
    let Right pp = principlePair (Lam "x" $ Lam "y" $ Var "y") in pp
    -- (Env [],TVar "a" :-> (TVar "b" :-> TVar "b"))
    let Left err = principlePair (Var "x" :@ Var "x") in err
    -- "Can't unify (TVar \"a\") with (TVar \"a\" :-> TVar \"b\")!"
    

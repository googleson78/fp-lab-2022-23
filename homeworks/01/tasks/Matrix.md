[relevant clip (w/ timestamp)](https://youtu.be/zHU1xH6Ogs4?t=48)
## Матрици

В това домашно ще реализирате малка библиотека за матрици, като по-интересното е, че ще
представяме матрици използвайки функции.

Това правим, защото

0. За чисто "упражнителни цели" - ще си поиграете с функции от по-висок ред и също ще разгледате нов начин за представяне на данни - чрез функции.
1. Предлага повече гаранции - ако използваме списък от списъци, не знаем нищо за размера на матрицата или пък за това дали всички редове имат еднаква дължина
2. По-удобно е, отколкото да правим наредена n-торка, а пък и по-гъвкаво, ако направим матриците си полиморфни по "размера" им.

Опитайте се да имплементирате всичко тук **без** да използвате списъци.

### `Three`

```haskell
data Three = Zero | One | Two
```

Тип данни с три стойности - `Zero`, `One`, `Two`.
Ще го използваме главно като индекс/ключ, подобно на това как ползваме числата за индекс/ключ в масив.

Има "имплементирано равенство", така че можем да използваме `(==)` оператора за него, ако ни се наложи.

Имаме също и

```haskell
flipThree :: Three -> Three
flipThree Zero = Two
flipThree One = One
flipThree Two = Zero
```

Което "обръща" стойността спрямо реда ѝ. Оказва се полезно за някои сметки.

### `Thrice`

```haskell
type Thrice a = Three -> a
```

Тип данни, който представя "три стойности от тип `a`" използвайки функция.

Ако имаме `f :: Thrice a`, то трите `a`-та можем да "получим" извиквайки съответно `f Zero`, `f One` и `f Two`.

Или по друг начин казано, `Thrice` е нещо(функция), което по даден индекс ни дава стойността която "седи" на този индекс.

#### Пример за `Thrice`

```haskell
t :: Thrice Int
t Zero = 10
t One = 100
t Two = 1000

> t Zero + t Two - t One
910
```

#### Еквивалентност с наредена тройка с компоненти от еднакъв тип

Това е еквивалентно на

```haskell
data Triple a = MkTriple a a a
```

Еквивалентността можем да покажем по следния начин:

```haskell
thriceToTriple :: Thrice a -> Triple a
thriceToTriple f = MkTriple (f Zero) (f One) (f Two)

tripleToThrice :: Triple a -> Thrice a
tripleToThrice (MkTriple x y z) ix =
  case ix of
    Zero -> x
    One -> y
    Two -> z
```

Вдъхновявайки се от това съответствие, имаме и функция, която го обръща във форма, която лесно се показва в `ghci`, тъй като
функциите по подразбиране не могат да бъдат принтени:

```haskell
thriceToTriple :: Thrice a -> (a, a, a)
thriceToTriple t = (t Zero, t One, t Two)
```

Имаме и по-обикновено изглеждаща функция, която конструира този тип, взимайки три стойности.
Удобна е главно за да тестваме неща по-лесно.

```haskell
thrice :: a -> a -> a -> Thrice a
thrice x y z ix =
  case ix of
    Zero -> x
    One -> y
    Two -> z
```

### `Matrix`

```haskell
newtype Matrix a = MkMatrix {getMatrix :: Thrice (Thrice a)}
```

#### `newtype`

Засега можем да си мислим че `newtype` прави същото като `data`, като по-късно на упражнение ще разясним разликата.

Тук използваме `newtype` вместо `type`, защото искаме типовата система да различава
между `Thrice (Thrice a)` и `Matrix a`, за да можем да представим абстрактен интерфейс за работа
с външния свят.
Също в `Haskell` се оказва удобно да имаш отделен тип данни, когато искаш да правиш определени неща, например да промениш как се показват стойностите в `ghci`.

#### Описание на типа данни

Матрица, представена чрез "ред от редове" - в този случай инцидентно представяме ред чрез типа `Thrice`.

Или ако разкрием типовия синоним, виждаме че това е `Three -> Three -> a` - по дадени два индекса,
получаваме елемент на матрицата.

Подобно на `Thrice`, елементите получаваме, като извикаме функцията, съдържаща се в `MkMatrix` конструктора.

Например, ако искаме елемента на ред 2 и колонка 3 от матрицата `m`, можем да я извикаме по следния начин - `m One Two`.

Един начин да си мислим за конструирането на `Matrix` е чрез въпроса
"Като са ми дадени два индекса в матрица, каква стойнст искам да седи зад тези индекси?".

Да си представим че искаме да изградим матрицата в която на индекс `i` `j` седи стойността `i ^ j`.
Това можем да направим по следния начин:

```haskell
threeToInteger :: Three -> Integer
threeToInteger Zero = 0
threeToInteger One = 1
threeToInteger Two = 2

expIndicesMatrix :: Matrix Integer
expIndicesMatrix =
  MkMatrix $ \i j -> threeToInteger i ^ threeToInteger j

> expIndicesMatrix
1 0 0
1 1 1
1 2 4
```

Както с `thrice`, тук имаме функция, с която по-удобно да конструираме матрици:

```haskell
matrix ::
  a -> a -> a ->
  a -> a -> a ->
  a -> a -> a ->
  Matrix a
matrix x0 x1 x2 y0 y1 y2 z0 z1 z2 =
  MkMatrix $ \i ->
    case i of
      Zero -> thrice x0 x1 x2
      One -> thrice y0 y1 y2
      Two -> thrice z0 z1 z2
```

Хубаво е сега да отбележим следния образец за имплементация, защото ще се повтаря сравнително често в задачите:

Функционалността за `Matrix` е имплементирана, използвайки аналогичната ѝ за `Thrice`.

Виждаме как всеки отделен ред на матрицата построяваме използвайки `thrice`.

#### Аналогия между `Map` структурите от данни и фунцкиите

Нека имаме някакъв тип `Map k v`, който държи стойности от тип `v` зад ключове от тип `k`,
т.е. нещо подобно на `std::unordered_map<k,v>` или `std::map<k,v>` от `C++`.

Тогава има доста директна аналогия между `Map k v` и `k -> v` - и двете са начини по дадено `k`
да получим `v`. Разликата между двете се изразява в това че `Map k v` е задължително краен обект,
докато `k -> v` може да "съдържа" безкрайно много стойностти (например `even :: Integer -> Bool`, която за всеки от безкрайното количество `Integer`-и ще ни върне `Bool`).

Имайки тази аналогия наум вече не е толкова странно представянето, което използваме:
3x3 матриците можем да представим като `Map Three (Map Three a)`, което съответства на
`Three -> (Three -> a)`.

#### Бележка за показването на матриците

Имате за задача(`showMatrix`) да имплементирате принтене на матрици.
От това следват две съществени неща:

* във всичките примери, които давам по-надолу, ще съм си избрал моята собствена имплементация
  на принтене, за да демонстрирам резулт от функции, но не е нужно вашата да е същата.
* преди да я решите, няма да можете да пробвате неща в `ghci` или да виждате изхода
  от тестовете си. Не би трябвало да е проблем, защото тя е една от първите задачи, но въпреки това, ако ви се наложи, можете да прескочите до нея.

## Задачи

### 1т. `constantMatrix :: a -> Matrix a`

Конструираите матрица, която на всички индекси има подадената стойност.

##### Примери

```haskell
> constantMatrix 'E'
'E' 'E' 'E'
'E' 'E' 'E'
'E' 'E' 'E'
```

### 0.5т. `diagonalMatrix :: a -> Matrix (Maybe a)`

Конструирайте матрица, която по главния си диагонал си има подадения елемент(`Just`),
а на всички други места няма нищо (`Nothing`).

#### Примери

```haskell
> diagonalMatrix True
Just True Nothing Nothing
Nothing Just True Nothing
Nothing Nothing Just True
```

### 0.5т. `otherDiagonalMatrix :: a -> Matrix (Maybe a)`

Конструирайте матрица, която по не-главния си диагонал си има подадения елемент(`Just`),
а на всички други места няма нищо (`Nothing`).

#### Примери

```haskell
> otherDiagonalMatrix False
Nothing Nothing Just False
Nothing Just False Nothing
Just False Nothing Nothing
```

### 1т. `addMatrix :: Matrix Integer -> Matrix Integer -> Matrix Integer`

Съберете подадените матрици покомпонентно.

#### Примери

```haskell
m1 =
  matrix
    1 2 3
    4 5 6
    7 8 9
m2 =
  matrix
    (-1) (-2) (-3)
    (-4) (-5) (-6)
    (-7) (-8) (-9)

> addMatrix m1 m2
0 0 0
0 0 0
0 0 0
```

### 1т. `showMatrix :: (a -> String) -> Matrix a -> String`

По подадена функция за конвертиране до низ на елемент, конвертирайте цялата матрица до низ.

След като имплементирате тази функция ще можете да виждате в `ghci` и от тестовете
стойности на матрици.

Не се притеснявайте за умно решение - засега е ок да го направите възможно най-"дървено".

#### Примери

Няма примери(нито пък тестове) тъй като имате свободата да си изберете как ще показвате матрици,
но за примерно показване може да видите всички други примери. (hehe)

### 0.5т. `ix :: Three -> Three -> Matrix a -> a`

По дадени два индекса, извадете от матрицата елементът, който седи на тези два индекса.

#### Примери

```haskell
m =
  matrix
    1 2 3
    4 5 6
    7 8 9
> ix One Two m
6
> ix Zero One m
2
```

### 0.5т. `getRow :: Three -> Matrix a -> Thrice a`

По даден индекс, върнете съответния му ред от подадената матрица.

#### Примери

```haskell
m =
  matrix
    1 2 3
    4 5 6
    7 8 9
> thriceToTriple $ getRow Two m
(7,8,9)
```

### 0.5т. `getCol :: Three -> Matrix a -> Thrice a`

По даден индекс, върнете съответната му колона от подадената матрица.

#### Примери

```haskell
m =
  matrix
    1 2 3
    4 5 6
    7 8 9
> thriceToTriple $ getCol One m
(2,5,8)
```

### 0.5т. `getDiag :: Matrix a -> Thrice a`

Върнете главния диагонал на матрицата.

#### Примери

```haskell
m =
  matrix
    1 2 3
    4 5 6
    7 8 9
> thriceToTriple $ getDiag m
(1,5,9)
```

### 0.5т. `getOtherDiag :: Matrix a -> Thrice a`

Върнете не-главния диагонал на матрицата.

#### Примери

```haskell
m =
  matrix
    1 2 3
    4 5 6
    7 8 9
> thriceToTriple $ getOtherDiag m
(3,5,7)
```

### 0.5т. `transpose :: Matrix a -> Matrix a`

Транспонирайте матрицата.

#### Примери

```haskell
m =
  matrix
    1 2 3
    4 5 6
    7 8 9
> transpose m
1 4 7
2 5 8
3 6 9
```

### 2т. `foldThriceWith :: (a -> a -> a) -> Thrice a -> a`

По даден `Thrice a`, комбинирайте всичките му стойности използвайки подадената
двуместна операция.

Тази операция има и още една интерпретация, която се оказва често полезна и е малко по-ясна, ако
разпънем `Thrice` синонима и преименуваме функцията:
`mergeMapWith :: (a -> a -> a) -> (Three -> a) -> a`

Взимаме две функции, `op :: a -> a -> a` и `f :: Three -> a` -

* чрез `f` казваме към какво ще преобразуваме всяка стойност на `Three`
* чрез `op` казваме как да комбинираме резултатите на `f`

Има _още един_ по-императивен начин да си мислим за това, който е че за всеки индекс(`Zero`, `One`, `Two`) изпълняваме някаква операция (`f`), използвайки `op` за да комбинираме резултатите накрая.

#### Примери

```haskell
> t = thrice True False True
> foldThriceWith (&&) t
False

> t = thrice True True True
> foldThriceWith (&&) t
True

> foldThriceWith (&&) $ \ix -> even (threeToInt ix)
False

> foldThriceWith (+) $ \ix -> threeToInt ix
3

> foldThriceWith (+) threeToInt
3
```

### 2т. `foldMatrixWith :: (a -> a -> a) -> Matrix a -> a`

Аналогично на функцията за `Thrice`, комбинирайте всичките стойности на матрицата изполвайки
подаденете двуместна операция.

Всичките интерпретации от `foldThriceWith` важат и тук.

#### Примери

```haskell
> MkMatrix mf = matrix 1 2 3 4 5 6 7 8 9
> foldMatrixWith (&&) (MkMatrix (\i j -> even (mf i j)))
False
> foldMatrixWith (&&) $ MkMatrix (\i j -> even (mf i j))
False
> foldMatrixWith (&&) $ MkMatrix $ \i j -> even (mf i j)
False
> foldMatrixWith (&&) $ MkMatrix $ \i j -> even $ mf i j
False
> foldMatrixWith (&&) $ MkMatrix $ \i j -> mf i j < 10
True
> foldMatrixWith (+) $ MkMatrix mf
45
> MkMatrix mf1 = matrix 1 2 3 4 5 6 7 8 9
> MkMatrix mf2 = matrix (-1) (-2) (-3) (-4) (-5) (-6) (-7) (-8) (-9)
> foldMatrixWith (+) $ MkMatrix $ \i j -> mf1 i j + mf2 i j
0
```

### 2т. `eqMatrix :: (a -> a -> Bool) -> Matrix a -> Matrix a -> Bool`

По подадена функция за равнество (`(a -> a -> Bool)`) за `a`-та и две матрици от `a`-та,
върнете дали двете матрици са равни.

#### Примери

```haskell
> m1 = matrix 1 2 3 4 5 6 7 8 9
> m2 = matrix (-1) (-2) (-3) (-4) (-5) (-6) (-7) (-8) (-9)
> eqMatrix (==) (addMatrix m1 m2) (constantMatrix 0)
True
```

### 1т. `imapThrice :: (Three -> a -> b) -> Thrice a -> Thrice b`

По подадена функция, която по елемент(`a`) и индекса му в дадения `Thrice` връща `b`,
превърнете `Thrice a` към `Thrice b`.

`i`-то идва от "индексиран".

#### Примери

```haskell
> threeToInt Zero = 0; threeToInt One = 1; threeToInt Two = 2
> f ix x = threeToInt ix * x
> thriceToTriple $ imapThrice f $ thrice 1 2 3
(0,2,6)
```

### 0.5т. `mapThrice :: (a -> b) -> Thrice a -> Thrice b`

По подадена функция обръщаща `a`-та към `b`-та, трансформирйате `Thrice a` към `Thrice b`.

#### Примери

```haskell
> thriceToTriple $ mapThrice even $ thrice 1 2 3
(False,True,False)
```

### 3т. `imapMatrix :: (Three -> Three -> a -> b) -> Matrix a -> Matrix b`

Аналогично на `imapThrice`, но сега с два индекса.

#### Примери

```haskell
> imapMatrix f $ matrix 1 2 3 4 5 6 7 8 9
(Zero,Zero,1) (Zero,One,2) (Zero,Two,3)
(One,Zero,4) (One,One,5) (One,Two,6)
(Two,Zero,7) (Two,One,8) (Two,Two,9)
```

### 0.5т. `mapMatrix :: (a -> b) -> Matrix a -> Matrix b`

Аналогично на `mapThrice`, но за матрици.

#### Примери

```haskell
> isNothing Nothing = True; isNothing (Just _) = False
> mapMatrix isNothing $ diagonalMatrix 42
False True True
True False True
True True False
```

### 1т. `place :: Three -> Three -> a -> Matrix a -> Matrix a`

```haskell
> place i j x m
```

На индекс `i` `j` в матрицата `m` поставете стойността `x`.

Помислете дали някоя от предишните функции може да бъде използвана за целта.

#### Примери

```haskell
> m = matrix 1 2 3 4 5 6 7 8 9
> m
1 2 3
4 5 6
7 8 9
> place Two One 42 m
1 2 3
4 5 6
7 42 9
> place One Zero 69 m
1 2 3
69 5 6
7 8 9
```

### 1т. `concatThriceWith :: String -> Thrice String -> String`

Слепете низовете от подадения `Thrice String`, като използвате подадения низ като разделител.

#### Примери

```haskell
> concatThriceWith " (my name is) " $ thrice "hi" "what" "who"
"hi (my name is) what (my name is) who"
```

### 2т. `concatMatrixWith :: String -> String -> Matrix String -> String`

```haskell
concatMatrixWith rowSep colSep m
```

Аналогично на `concatThriceWith`, но използвайте `rowSep` за разделител между отделните редове,
а `colSep` за разделител между елементите в рамките на един ред.

#### Примери

```haskell
> concatMatrixWith "__" "^^^" $ mapMatrix show $ matrix 1 2 3 4 5 6 7 8 9
"1^^^2^^^3__4^^^5^^^6__7^^^8^^^9"
```

### 1т. `showMatrixComposition :: (a -> String) -> Matrix a -> String`

Преимплементирайте показване на матрици както за `showMatrix`, но използвайки новите функции за работа с матрици.

## 8т. Бонус задачи

Измислете тип

```haskell
type GenericMatrix ... a = ???
```

който да ни позволява да изразим(подобно на досегашните) матрици, които не са задължително с размер `3x3`.
Целим размерът им да се кодира в типовата система.

`...` можете да запълните с колкото искате на брой неща.

### Конструиране

За удобство по време на имплементиране препоръчвам да направите функция която по списък от списъци конструира този тип данни,
въпреки че няма как тази операция да е тотална (например ако дължината на списъка не е правилна), попълвайки пропуските с `error <assert>`.

Ако не бяхме в домашно, такава функция не би следвало да е част от публичния интърфейс на билблиотеката ни.

Ако ви хрумне друг начин който е по-безопасен и подобно удобен(по-малко места на които има runtime грешки) съм отворен към идеи.

### fold

Измислете как да имплементирате съответната `fold` функция за тези матрици, добавяйки допълните аргументи, които смятате, че са ви нужни.

Целим да имаме функция която взима **поне** операция `(m -> m -> m)` и комбинира стойностите които се съдържат в матрицата ни използвайки тази операция.

```haskell
foldGenericMatrix ::
  ... ->
  (m -> m -> m) ->
  ... ->
  GenericMatrix ... m ->
  ... ->
  m
```

### map

Реализирайте `map` функция за `GenericMatrix`, т.е. нещо, което взима поне `(a -> b)` и `GenericMatrix`, съдържащо `a`-та и връща
`GenericMatrix`, съдържащо `b`-та.

```haskell
mapGenericMatrix ::
  ... ->
  (a -> b) ->
  ... ->
  GenericMatrix ... a ->
  ... ->
  GenericMatrix ... b
```

### Умножение

Помислете как да имплементиране умножение на матрици използвайки този тип данни.

```haskell
multGenericMatrix ::
  ... ->
  GenericMatrix ... Integer ->
  ... ->
  GenericMatrix ... Integer ->
  ... ->
  GenericMatrix ... Integer ->
```

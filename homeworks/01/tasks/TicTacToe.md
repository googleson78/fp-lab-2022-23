## Морски шах

В това домашно ще използваме билбиотеката за матрици, за да релизираме някои функционалности на играта на морски шах ("хикс и о").

### `Marker`

```haskell
data Marker = X | O
  deriving (Eq, Show)
```

Тип, с който изразяваме двете различни неща, които традиционно се използват в играта на морски шах.
Можете да използвате `==` оператора за тях.

### `Spot`

```haskell
type Spot = Maybe Marker
```

Удобен типов синоним за да не пишем много пъти `Maybe Marker`.

Изразява едно място от играта на морски шах.
`Nothing` стойността използваме, за да индикираме, че на това място все още не е поставен маркер.

### `Board`

```haskell
type Board = Matrix Spot
```

Отново, удобен типов синоним, за да не пишем много пъти `Matrix Spot`.

Изразява самото игрално поле за морски шах, използвайки библиотеката ни за матрици.

### `Result`

```haskell
data Result = Full | HasEmpty | Wins Marker
```

Изразява какви са възможните резултати по подадени дъска или ред.

* `Full` - пространството е запълнено, не можем да слагаме повече неща, но в същото време няма победител.
  Ако това е резултатът за цялата дъска, то играта е завършила в равенство.
* `HasEmpty` - пространството има още празни места, можем да продължим да играем
* `Wins m` - в това пространство е спечелил `m`, т.е. има 3 поредни `m`-та някъде в пространството.

Тъй като типът ни за дъска не е достатъчно специфичен, за да забрани да се случи нещо такова

```haskell
X X X
O O O
_ _ _
```

Ще поставим предусловието, че такива дъски _не се случват_ и ще се преструваме, че можем просто да игнорираме такива ситуации. (в стил C/C++ undefined behaviour)

Конкретно, това означава за вас, че ако попаднете в ситуация, в която има двама възможни победители, е ок да изберете който и да е от двата.

Постарал съм се тестовете да **не** разчитат на избора на победител в такава ситуация, но може и да имам грешки, така че пишете, ако имате съмнение, че се случва нещо такова.

## Задачи

### 2т. `join :: Result -> Result -> Result`

Имплемнетирайте операция които "слива" две `Result` стойности.

Идеята тук е да си мислим за каква операция ни е нужна, за да можем да вземем поотделно резултатите от всеки ред
на даден `Board` и после да ги слеем, така че да получим "валиден" резултат за целия `Board`.

#### Примери

<нарочно оставено празно, защото иначе ще ви реша задачата>

### 1т. `checkThreeSpots :: Thrice Spot -> Result`

Сметнете резултата за дадени три места.

#### Примери

```haskell
> checkThreeSpots $ thrice (Just X) (Just X) (Just X)
Wins X
> checkThreeSpots $ thrice (Just X) (Just X) Nothing
HasEmpty
> checkThreeSpots $ thrice (Just X) (Just X) (Just O)
Full
```

### 3т. `winner :: Board -> Result`

Пресметнете кой печели по подадена дъска. `join` може да е полезна тук.

#### Примери

```haskell
m =
  matrix
    (Just X) (Just X) (Just X)
    (Just O) (Just X) (Just O)
    (Just O) (Just O) (Just O)
> winner m
Wins X

m =
  matrix
    (Just O) (Just X) (Just X)
    (Just O) (Just X) (Just O)
    (Just O) (Just O) (Just O)
> winner m
Wins O

m =
  matrix
    (Just O) (Just X) (Just O)
    (Just O) (Just X) (Just X)
    (Just X) (Just O) (Just O)
> winner m
Full

m =
  matrix
    (Just O) (Just X) (Just O)
    (Just O) (Just X) (Just X)
    (Just X) (Just O) Nothing
> winner m
HasEmpty
```

### 4т. `emptySpots :: Board -> [(Three, Three)]`

Върнете индексите на всичките празни места за дадената дъска.

#### Примери

```haskell
m =
  matrix
    (Just O) Nothing (Just O)
    Nothing (Just X) (Just X)
    (Just X) (Just O) Nothing
> emptySpots m
[(Zero,One),(One,Zero),(Two,Two)]

m =
  matrix
    (Just O) (Just X) (Just O)
    (Just O) (Just X) (Just X)
    (Just X) (Just O) (Just O)
> emptySpots m
[]
```

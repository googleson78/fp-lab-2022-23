{-# LANGUAGE NamedFieldPuns #-}

data RPS = Rock | Paper | Scissors

data Nat = Zero | Succ Nat

-- let <bindings> in <expr>
quad :: Int -> Int
quad x = (let x = 10 in 10 + pesho) + double (double x)
  where
    double :: Int -> Int
    double x = x * x
      where
        y = 30

    pesho = 5
    gosho = 10

quad' :: Int -> Int
quad' x =
  let double :: Int -> Int
      double x = x * x

      pesho = 5
   in pesho + double (double x)

-- struct Skill {
--  Category category;
--  int rating;
-- };
data Skill = MkSkill
  { category :: Category,
    rating :: Int
  }

passCheck :: Skill -> Int -> Bool
passCheck MkSkill {category, rating} x = rating > x

createSkill :: Category -> Int -> Skill
createSkill pesho gosho =
  MkSkill
    { category = pesho,
      rating = gosho
    }

data Category = Psyche | Physical

# HaskellSamples

- [Functors, Applicatives, And Monads In Pictures](http://adit.io/posts/2013-04-17-functors,_applicatives,_and_monads_in_pictures.html)

### Monads:
```haskell
f :: a -> m b
f :: a -> Maybe b            -- m = Maybe
f :: a -> [] b               -- m = []
f :: a -> (Either s) b       -- m = Either s
f :: a -> ((,) s) b          -- m = ((,) s)
f :: a -> ((->) e) b         -- m = ((->) e)
f :: a -> (State s) b        -- m = State s
f :: a -> IO b               -- m = IO
```

```haskell
class Applicative m => Monad (m :: * -> *) where
    return :: a -> m a
    (>>=) :: m a -> (a -> m b) -> m b
    (>>) :: m a -> m b -> m b
    x >> y = x >>= (\_ -> y)
    fail :: String -> m a
```

 1.  `return a >>= k == k a`
 2.  `m >>= return == m`
 3.  `(m >>= k) >>= k' == m >>= (k >>= k') -- m >>= (\x -> k x >>= k')`
                          

### Courses:

- [Денис Николаевич Москвин](http://mit.spbau.ru/lecturers/%D0%BC%D0%BE%D1%81%D0%BA%D0%B2%D0%B8%D0%BD-%D0%B4%D0%B5%D0%BD%D0%B8%D1%81-%D0%BD%D0%B8%D0%BA%D0%BE%D0%BB%D0%B0%D0%B5%D0%B2%D0%B8%D1%87) - twitter [@deniok](https://twitter.com/deniok)
  - [Функциональное программирование на языке Haskell - Часть 1](https://stepik.org/course/75/syllabus) (see also: [Certificate](https://stepik.org/certificate/6b271b1181c9aba4609fa53f15e0ebfcb6210087.pdf), [Solutions](https://github.com/kogoia/HaskellSamples)) - stepik.org 
  - [Функциональное программирование](https://compscicenter.ru/courses/func-prog/2015-spring/about/) ([Практические задания](http://mit.spbau.ru/sewiki/index.php/%D0%A4%D1%83%D0%BD%D0%BA%D1%86%D0%B8%D0%BE%D0%BD%D0%B0%D0%BB%D1%8C%D0%BD%D0%BE%D0%B5_%D0%BF%D1%80%D0%BE%D0%B3%D1%80%D0%B0%D0%BC%D0%BC%D0%B8%D1%80%D0%BE%D0%B2%D0%B0%D0%BD%D0%B8%D0%B5_2015)) - youtube.com

![haskell type hierarchy](https://i.stack.imgur.com/TrbOX.jpg)

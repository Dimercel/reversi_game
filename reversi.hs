#!/usr/bin/env stack
-- stack --resolver lts-8.0 script

-- | Данный модуль реализует функционал настольной игры Реверси.
-- Содержит в себе все необходимое для имитации партии этой игры.

import qualified Data.Set as Set
import Data.List (all, inits, find, nub)
import Data.Maybe (fromMaybe)


-- | ЧТО НЕОБХОДИМО ДЛЯ ИГРЫ.
--
--
--   Прежде всего - нужна квадратная доска с 8*8 клетками. Для игры может сгодиться
-- и шахматная - доска, но стоит помнить о различиях в координатах.

boardSize :: Int
boardSize = 8

-- | На шахматной доске нумерация строк начинается с самой нижней строки, в
-- Реверси же она начинается с самого верха. Но для непосредственного использования
-- в коде, мы будем использовать более приемлемую систему координат. Она будет
-- описывать координаты клетки в терминах позиций строк и столбцов.

-- Первое число - это строка, второе - столбец.
type Position = (Int, Int)

-- | Стоит заметить что тип Position не совпадает с традиционной нотацией
-- записи ходов. В традиционной записи сначала идет координата столбца, а
-- после - строки. Мы же используем обратную нотацию - сначала индекс строки,
-- потом столбца.
--   Индексация строк и столбцов начинается с нуля. Приведем несколько
-- примеров чтобы рассеять все сомнения: d5 -> (4, 3), a1 -> (0,0),
-- h8 -> (7,7).

-- | Далее расскажем о главных персонажах игры - фишках. Основой игры служат
-- два типа фишек контрастных цветов. Кто-то из соперников должен первым
-- выбрать цвет фишек за который ему предстоит играть, тогда второй соперник
-- будет играть оставшимся цветом.

data SideId = White | Black deriving (Show, Eq, Ord)

-- | Так как в игре участвуют только два соперника, то мы
-- легко можем определить сторону противоположную указанной.

invertSide :: SideId -> SideId
invertSide Black = White
invertSide White = Black

-- | В ходе партии, фишки выставляются на доску поочередно, каждым
-- из соперников по одной за раз. Фишка определяет принадлежность
-- к игроку и располагается на одной из 64 возможных позиций.

data Disk = Disk Position SideId deriving (Show, Eq)

diskPos :: Disk -> Position
diskPos (Disk pos _) = pos

diskSide :: Disk -> SideId
diskSide (Disk _ side) = side

instance Ord Disk where
  (<=) (Disk p1 c1) (Disk p2 c2) = p1 <= p2 && c1 <= c2

-- | В заключение рассмотрим сущность, хранящую в себе состояние игры. Это одна
-- из ключевых концепций в нашей реализации. Ее задачей является хранение
-- информации о всех фишках, присутствующих на доске.
--   Здесь можно было поступить прямолинейно и хранить фишки в двумерном массиве
-- размера 8*8, вне зависимости есть на данной клетке фишка или нет. Мы же хотим
-- более гибкого решения и в общем случае не ограниченного размером доски. Поэтому
-- в нашей реализации мы храним лишь те фишки, что присутствуют на доске.
-- Информацию о пустых клетках мы не храним, так как понятно что если на указанной
-- позиции нет фишки, то она пуста.
type GameState = Set.Set Disk

diskByPos :: GameState -> Position -> Disk
diskByPos state pos = head $ Set.elems $ Set.filter (\x -> diskPos x == pos) state


-- | ДОПОЛНИТЕЛЬНОЕ ПОСТРОЕНИЕ.
--
--
--   Прежде чем рассказать об основных правилах игры введем одно дополнительное
-- построение, оно нам облегчит дальнейшее повествование. В рамках этой задачи
-- мы введем вспомогательный тип Direction, который определяет направление сторон
-- света и ряд вспомогательных функций для работы с ним.
--   Целью всего этого является построение абстракции над процессом взятия фишек. В
-- соответствии с правилами игры, фишки могут быть "взяты", если они формируют
-- непрерывный ряд по горизонтали, вертикали или диагонали. Мы же попробуем и тут
-- добиться большей гибкости и представим этот аспект как отношение "соседних"
-- клеток. Тогда, изменение лишь в одной функции приведут к безболезненному
-- изменению этого элемента игры.

-- | Начнем с вспомогательного типа Direction. Его задача - это определение
-- направления. Так как в игре используются горизонтальные, вертикальные и
-- диагональные ряды, то здесь идеально подходят стороны света.
--   Север и Юг будут отвечать за вертикальное направление, а
-- Запад и Восток за горизонтальное. Промежуточные же стороны света будут
-- отвечать за диагональные направления.
data Direction = N | NE | E | SE | S | SW | W | NW deriving (Show)

-- | Теперь же нам нужно как-то связать направления света с координатами доски.
-- Сделаем мы это достаточно прямолинейно и просто, каждому направлению света
-- сопоставим требуемое приращение координат, которое необходимо для перехода на
-- одну клетку в указанном направлении.
dirInc :: Direction -> (Int, Int)
dirInc d =
  case d of
    N  -> (-1, 0)
    NE -> (-1, 1)
    E  -> (0, 1)
    SE -> (1, 1)
    S  -> (1, 0)
    SW -> (1, -1)
    W  -> (0, -1)
    NW -> (-1, -1)

-- | Далее определим функцию, которая будет непосредственно
-- выполнять один шаг из текущей позиции в указанном направлении.
stepInDirect :: Position -> Direction -> Position
stepInDirect (row, col) dir = (row + fst (dirInc dir),
                               col + snd (dirInc dir))

-- | Как же происходит перемещение в определенном направлении?
-- Давайте посмотрим! Попробуем пройти два шага на "Юг" из
-- начальной позиции (0, 0):
-- stepInDirect (stepInDirect (0,0) S) S => (2,0)
-- В результате мы переместились на строку с индексом 2!

-- | Ну и наконец-то, о чем уже говорилось ранее, а именно определение
-- "соседних" клеток. Здесь ничего сложного, все в соответствии с
-- правилами игры. Соседними считаются те клетки, которые граничат с
-- текущей по горизонтали, вертикали или диагонали.
--   Как же мы можем описать это используя наше дополнительное построение?
-- Это не составит большого труда: список "соседних" позиций состоит из
-- всех позиций находящихся в шаговой доступности от указанной позиции.
neighbors :: Position -> [Position]
neighbors pos = map (stepInDirect pos) [N, NE, E, SE, S, SW, W, NW]

-- | Рассмотрим пример использования функции neighbors:
-- neighbors (0, 0) => [(-1, 0), (-1, 1), (0, 1), (1, 1),
--                      (1, 0), (1, -1), (0, -1), (-1, -1)]
--   Как видно из результата, в списке присутствуют отрицательные координаты
-- позиций. Стоит помнить что такие позиции не допустимы в игре!


-- | ОСНОВНЫЕ ПРАВИЛА ИГРЫ.
--
--
--   После описания основных примитивов игры настало время поговорить
-- о том что можно и чего нельзя. Итак, расскажем о правилах игры.
--
-- - Ход выполняется только на пустую позицию доски.
posIsEmpty :: GameState -> Position -> Bool
posIsEmpty state pos =
  Set.null $ Set.filter (\x -> diskPos x == pos) state

-- - Ходы, с координатами отсутствующими на доске не допустимы
correctPos :: Position -> Bool
correctPos (row, col)
  | row < 0 = False
  | col < 0 = False
  | row > (boardSize - 1) = False
  | col > (boardSize - 1) = False
  | otherwise = True

-- - Ход допустим лишь в соседнюю с другой фишкой позицию
haveNeighbors :: GameState -> Position -> Bool
haveNeighbors state pos
  | not $ correctPos pos = False
  | otherwise =
    let neighPos = neighbors pos
    in not $ Set.null (Set.filter (\x -> diskPos x `elem` neighPos) state)

-- | Это необходимое, но не достаточное для совершения хода условие. Достаточное
-- условие дополнительно требует чтобы за ход была взята хотя бы одна вражеская
-- фишка. Если такого хода для игрока не существует, то он должен пропустить этот
-- ход.

-- - Начальное положение игры состоит из 4 фишек, по две на каждую сторону,
-- находящихся в центре доски.
beginState :: GameState
beginState = Set.fromList [
    Disk (3, 4) Black,
    Disk (4, 3) Black,
    Disk (3, 3) White,
    Disk (4, 4) White
  ]


-- | ОТКРЫТЫЙ ИНТЕРФЕЙС МОДУЛЯ.
--
--
--   Описав все сущности игры и их базовые операции, самое время заняться
-- интерфейсной частью модуля. Здесь мы должны задаться вопросом: "Какая
-- информация прежде всего интересует пользователя?".

-- Сколько на доске фишек принадлежащих указанной стороне?
sideDisksCount :: GameState -> SideId -> Int
sideDisksCount state side =
  length $ Set.filter (\x -> diskSide x == side) state

-- | Примеры:
-- sideDisksCount (beginState) White => 2
-- sideDisksCount (beginState) Black => 2


-- Сколько всего фишек на доске?
disksCount :: GameState -> Int
disksCount = Set.size

-- | Примеры:
-- disksCount beginState => 4


-- Какая из сторон лидирует в текущий момент?
--   Лидирующей стороной считается та, что выставила больше своих фишек на доску.
-- Если количество фишек равное, то лидер отсутствует.
whoIsLeader :: GameState -> Maybe SideId
whoIsLeader state =
  let blackCount = sideDisksCount state Black
      whiteCount = sideDisksCount state White
  in case (blackCount > whiteCount, whiteCount > blackCount) of
       (True, _) -> Just Black
       (_, True) -> Just White
       _ -> Nothing

-- | Примеры:
-- whoIsLeader beginState => Nothing


-- | Какие фишки будут "побиты", если указанная сторона сделает указанный ход?
--   Так просто на этот вопрос не ответить и нам придется прежде разбить его на
-- подвопросы. Затем, решив каждый из них, мы сможем ответить на основной.
--   Начнем пожалуй с описания ряда фишек. О ряде мы уже говорили в раздела с
-- дополнительным построением, но строгой формулировки не давали. Исправим это!
-- Рядом мы назовем непрерывную цепочку из фишек на доске, которая начинается с
-- какой-нибудь фишки, а продолжается только лишь в указанном направлении.
--   Вспомним нашу функцию stepInDirect - она как раз порождает ряды, но оперирует
-- позициями на доске, а не самими фишками. Рассмотрим примеры как с помощью нее
-- можно получить ряд позиций: take 6 $ iterate (`stepInDirect` S) (0,0) =>
-- => [(0,0),(1,0),(2,0),(3,0),(4,0),(5,0)]. Мы получили ряд позиций начинающийся
-- из клетки (0,0) и идущий в направлении "Юг".
disksRow :: GameState -> Position -> Direction -> [Disk]
disksRow state pos dir =
  let nextPos = stepInDirect pos dir
  in if posIsEmpty state pos
     then []
     else diskByPos state pos : disksRow state nextPos dir

-- | Примеры:
-- disksRow beginState (4, 3) N  => [Disk (4,3) Black, Disk (3,3) White]
-- disksRow beginState (6, 4) S  => []
-- disksRow beginState (4, 3) NE => [Disk (4,3) Black, Disk (3,4) Black]

-- | Что ж, ряд мы получили, теперь нам необходимо больше информации о нем. Нам
-- интересно может ли быть "взят" указанный ряд фишек, указанной стороной. В
-- соответствии с правилами, на концах такого ряда должны находиться фишки
-- указанной стороны, а между ними фишки противоположной стороны. Получается
-- что фишки как бы "зажаты" между двумя другими в ряде.
isBetween :: [Disk] -> SideId -> Bool
isBetween disks side
  | length disks < 3 = False
  | otherwise = and [
      diskSide (head disks) == side,
      all (\x -> diskSide x /= side) (init $ tail disks),
      diskSide (last disks) == side
    ]

-- | Примеры:
-- let state = Set.insert (Disk (2,3) Black) beginState
-- in isBetween (disksRow state (4, 3) N) Black => True

-- | Самое время "взять" ряд! Теперь мы обладаем всей необходимой информацией для
-- "взятия" ряда. Следует лишь учесть один момент - ряд может содержать в себе
-- подряд, пригодный для "взятия". В таком случае нам нужен самый меньший подряд.
deadRow :: [Disk] -> SideId -> [Disk]
deadRow disks side
  | not $ isBetween disks side = []
  | otherwise = fromMaybe [] $ find (`isBetween` side) (inits disks)

-- | Мы во всеоружии и готовы ответить теперь на основной вопрос. Нам нужны все
-- "побитые" фишки, если указанная сторона пойдет в указанную позицию.
whoWillKilled :: GameState -> SideId -> Position -> [Disk]
whoWillKilled state side pos
  | not $ posIsEmpty state pos = []
  | not $ correctPos pos       = []
  | otherwise =
    let newState = Set.insert (Disk pos side) state
        allRows  = map (disksRow newState pos) [N, NE, E, SE, S, SW, W, NW]
    in filter (\x -> diskSide x /= side) (concatMap (`deadRow` side) allRows)

-- | Примеры:
-- whoWillKilled beginState Black (2,3) => [Disk (3,3) White]
-- whoWillKilled beginState White (5,3) => [Disk (4,3) Black]


-- Допустим ли ход в указанную позицию для указанной стороны?
--   Ход считается допустимым если клетка пуста и при этом будет "взята" хоть
-- одна вражеская фишка.
isValidMove :: GameState -> SideId -> Position -> Bool
isValidMove state side pos = [] /= whoWillKilled state side pos

-- | Примеры:
-- isValidMove beginState White (5,3) => True
-- isValidMove beginState Black (0,0) => False


-- Какие позиции для хода допустимы указанной стороне?
--   Вспомним что допустимые ходы всегда должны быть "соседями" фишек, уже
-- выставленных на доску. Тогда, все сводится к проверке таких позиций на
-- предмет корректного хода.
validMoves :: GameState -> SideId -> [Position]
validMoves state side =
  let allNeighbors = concatMap (neighbors . diskPos) (Set.elems state)
      onlyEmptyCells = filter (posIsEmpty state) allNeighbors
  in nub $ filter (isValidMove state side) onlyEmptyCells

-- | Примеры:
-- validMoves beginState Black [(2,3),(3,2),(4,5),(5,4)]
-- validMoves beginState White [(2,4),(4,2),(3,5),(5,3)]


-- Существует ли хоть один допустимый ход для указанной стороны?
moveExists :: GameState -> SideId -> Bool
moveExists state side = [] /= validMoves state side

-- Как будет выглядеть игровое состояние, если указанная сторона
-- сделает ход в указанную позицию?
--   Фактически это совершение хода указанной стороной в указанную
-- позицию. Если же передается не корректный ход, то возвращается
-- переданное в качестве первого аргумента игровое состояние.
commitMove :: GameState -> SideId -> Position -> GameState
commitMove state side pos
  | not $ isValidMove state side pos = state
  | otherwise =
    let killed = whoWillKilled state side pos
        newDisk = Disk pos side
    in Set.unions [
      Set.singleton newDisk,
      Set.difference state (Set.fromList killed),
      Set.fromList (map (\(Disk pos _) -> Disk pos side) killed)
    ]

-- | Примеры:
-- commitMove beginState Black (2,3) =>
-- => Set.fromList [Disk (2,3) Black,Disk (3,3) Black,Disk (3,4) Black,
--                  Disk (4,3) Black,Disk (4,4) White]
-- commitMove beginState White (5,3) =>
-- => Set.fromList [Disk (3,3) White,Disk (4,3) White,Disk (5,3) White,
--                  Disk (3,4) Black,Disk (4,4) White]
-- commitMove beginState Black (0,0) == beginState => True

package ru.lester.fpinscala.ch6

import ru.lester.fpinscala.ch6._

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Machine {

  sealed trait Input
  case object Coin extends Input
  case object Turn extends Input

  import State._

  def simulateMachineMy(input: List[Input]): State[Machine, (Int, Int)] = {
    for {
    	_ <- sequence(input map {case Coin => insertCoin; case Turn => turnKnob })
        s <- get
    } yield (s.coins, s.candies)
  }

  def insertCoin: State[Machine, Unit] = modify(m => {
    if(m.candies == 0 || m.locked == false) m
    else m.copy(locked = false).copy(coins = m.coins + 1)
  })

  def turnKnob: State[Machine, Unit] = modify(m => {
    if(m.candies == 0 || m.locked) m
    else m.copy(locked = true).copy(candies = m.candies - 1)    
  })


  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    _ <- sequence(inputs.map(i => modify((s: Machine) => (i, s) match {
      case (_, Machine(_, 0, _)) => s
      case (Coin, Machine(false, _, _)) => s
      case (Turn, Machine(true, _, _)) => s
      case (Coin, Machine(true, candy, coin)) =>
        Machine(false, candy, coin + 1)
      case (Turn, Machine(false, candy, coin)) =>
        Machine(true, candy - 1, coin)
    })))
    s <- get
  } yield (s.coins, s.candies)
}
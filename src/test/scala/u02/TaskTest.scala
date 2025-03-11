package u02

import org.junit.*
import org.junit.Assert.*
import task5.Optionals.Optional.*
import u02.Lab2.*
import u02.Lab2.Expr.*

class TaskTest:


  @Test def testPositiveFun(): Unit =

    assertTrue(f(-2) match {case "negative" => true; case _ => false})
    assertTrue: // significant indentation style
      fPos(0) match
        case "positive" => true
        case _ => false

  @Test def testNegFun(): Unit =
    assertTrue:
      empty("") != neg(empty)("")

    assertTrue:
      empty("ciao") != fNeg(empty)("ciao")

  @Test def testGenericNegFun(): Unit =
    assertTrue:
      positive(3) != negGen(positive)(3)

    assertTrue:
      positive(-1) != negGen(positive)(-1)

  @Test def testCurrying1(): Unit =
    assertTrue(p1(2)(5)(5))
    assertFalse(p1(8)(5)(5))
    assertFalse(p1(2)(5)(4))
    
  @Test def testCurrying2(): Unit =
    assertTrue(p2(2,5,5))
    assertFalse(p2(8,5,5))
    assertFalse(p2(2,5,4))

  @Test def testCurrying3(): Unit =
    assertTrue(p3(2)(5)(5))
    assertFalse(p3(8)(5)(5))
    assertFalse(p3(2)(5)(4))

  @Test def testCurrying4(): Unit =
    assertTrue(p4(2, 5, 5))
    assertFalse(p4(8, 5, 5))
    assertFalse(p4(2, 5, 4))

  @Test def testCompose(): Unit =
    val compFun = compose(boolToInt,empty)
    assertTrue:
      compFun("") == 0
    assertTrue:
      compose(boolToInt,positive)(-1) == 1

  @Test def testComposeThree(): Unit =
    assertTrue:
      composeThree(positive, boolToInt, empty)("")

  @Test def testPower(): Unit =
    assertTrue(power(5,2) == 25)
    assertTrue(powerTail(2,3) == 8)
    
  @Test def  testReverseNumber(): Unit =
    assertEquals(reverseNumber(12345) ,54321)

  @Test def testAddExpr(): Unit =
    val simpleAdd = Expr.Add(Literal(5),Literal(97))
    assertEquals(102, evaluate(simpleAdd))

  @Test def testMultExpr(): Unit =
    val simpleMul = Expr.Multiply(Literal(5), Literal(9))
    assertEquals(45, evaluate(simpleMul))

  @Test def testEvaluateComplexExpr(): Unit =
    val expr = Expr.Add(Expr.Multiply(Literal(4), Expr.Add(Literal(7),Literal(2))), Literal(94))
    //Expected: 4*(7+2)+94 = 130
    assertEquals(130, evaluate(expr))

  @Test def testShowExpr(): Unit =
    val expr = Expr.Add(Expr.Multiply(Literal(4), Expr.Add(Literal(7), Literal(2))), Literal(94))
    assertEquals("(4*(7+2)+94)", show(expr))

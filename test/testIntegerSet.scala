

import org.scalatest.FunSuite

class TestIntegerSet extends FunSuite{
  test("IntegerSet should be empty"){
    val set = new IntegerSet();
    assert(set.ranges.length == 0);
  }
  test("Set should not contain duplicates"){
    val set = new IntegerSet();
    set.insert(1);
    set.insert(1);
    set.insert(1);
    set.insert(1);
    set.insert(1);
    assert(set.contains(1) && set.ranges.length == 1);
  }
  test("Set should merge intervals"){
    val set = new IntegerSet();
    set.insert(1);
    set.insert(2);
    set.insert(5);
    set.insert(4);
    set.insert(3);
    assert(set.ranges.length == 1)
  }
}


import scala.annotation.tailrec
import scala.annotation.tailrec

class IntegerSet {
	var ranges = List[Range]();
	
	def contains(n:Int): Boolean = {
	  ranges.foreach{ r =>
	    if(r.contains(n)){
	      return true;
	    }
	  }
	  return false;
	}
	
	def insert(n: Int): IntegerSet = {
	  
	  if(ranges.isEmpty){
	    ranges = List(n to n); 
	  }else{
		ranges = internal_insert(ranges,List(),n);
	  }
	  this;
	}
	@tailrec private def internal_insert(xs: List[Range],
	      ack: List[Range], n: Int) : List[Range] = {
	    xs match{
	      case List() => ack
	      case a :: list => {
	        if(a.contains(n)){
	          ranges
	        }else if(!list.isEmpty && (justAbove(a, n) && justBelow(list.head, n))) {
	          (combine(a,list.head) :: ack).reverse ++ list.tail;
	          }
	        else if(justAbove(a,n)){
	         (combine(a, n to n) :: ack).reverse ++ list
	       } else if(justBelow(a,n)){
	         (combine(n to n, a) :: ack).reverse ++ list
	       } else if(a.start > n){
	         ( a :: (n to n) :: ack).reverse ++ list
	       }else if(!list.isEmpty){
	         internal_insert(list,a::ack,n)
	       }else{
	         ((n to n) :: a :: ack).reverse ++ list
	       } 
	      }
	    }
	    	    
	  }
	
	def justBelow(a: Range, n: Int): Boolean = {
	  return a.start - 1 == n;
	}
	def justAbove(a: Range, n: Int): Boolean = {
	  return a.end + 1 == n;
	}
	def combine(a:Range, b:Range) = {
	  a.start to b.end;
	}
	override def toString():String = {
	  return ranges.toString;
	}
}
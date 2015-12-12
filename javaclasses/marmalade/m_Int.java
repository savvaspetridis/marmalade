package marmalade;

import jm.JMC;

public class m_Int implements JMC {
	
	int t;
	
	public m_Int(m_Int x) {
		t = x.get();
	}
	
	public m_Int(int x) {
		t = x;
	}
	
	public int get() {
		return t;
	}
	
	public void set(int x) {
		t = x;
	}
	
	// add
	public static int add(int x, int y) {
		return x + y;
	}
	
	public static int add(int x, m_Int y) {
		return x + y.get();
	}
	
	public static int add(m_Int x, int y) {
		return x.get() + y;
	}
	
	public static int add(m_Int x, m_Int y) {
		return x.get() + y.get();
	}
	
	// subtract
	public static int sub(int x, int y) {
		return x - y;
	}
	
	public static int sub(int x, m_Int y) {
		return x - y.get();
	}
	
	public static int sub(m_Int x, int y) {
		return x.get() - y;
	}
	
	public static int sub(m_Int x, m_Int y) {
		return x.get() - y.get();
	}
	
	// multiply
	public static int mult(int x, int y) {
		return x * y;
	}
	
	public static int mult(int x, m_Int y) {
		return x * y.get();
	}
	
	public static int mult(m_Int x, int y) {
		return x.get() * y;
	}
	
	public static int mult(m_Int x, m_Int y) {
		return x.get() * y.get();
	}
	
	// divide
	public static int divide(int x, int y) {
		return x / y;
	}
	
	public static int divide(int x, m_Int y) {
		return x / y.get();
	}
	
	public static int divide(m_Int x, int y) {
		return x.get() / y;
	}
	
	public static int divide(m_Int x, m_Int y) {
		return x.get() / y.get();
	}
	
	// less than
	public static boolean lt(int x, int y) {
		if (x < y) return true;
		return false;
	}
	
	public static boolean lt(int x, m_Int y) {
		if (x < y.get()) return true;
		return false;
	}
	
	public static boolean lt(m_Int x, int y) {
		if (x.get() < y) return true;
		return false;
	}
	
	public static boolean lt(m_Int x, m_Int y) {
		if (x.get() < y.get()) return true;
		return false;
	}
	
	// less than or equal to
	public static boolean leq(int x, int y) {
		if (x <= y) return true;
		return false;
	}
	
	public static boolean leq(int x, m_Int y) {
		if (x <= y.get()) return true;
		return false;
	}
	
	public static boolean leq(m_Int x, int y) {
		if (x.get() <= y) return true;
		return false;
	}
	
	public static boolean leq(m_Int x, m_Int y) {
		if (x.get() <= y.get()) return true;
		return false;
	}
	
	// greater than
	public static boolean gt(int x, int y) {
		if (x > y) return true;
		return false;
	}
	
	public static boolean gt(int x, m_Int y) {
		if (x > y.get()) return true;
		return false;
	}
	
	public static boolean gt(m_Int x, int y) {
		if (x.get() > y) return true;
		return false;
	}
	
	public static boolean gt(m_Int x, m_Int y) {
		if (x.get() > y.get()) return true;
		return false;
	}
	
	// greater than or equal to
	public static boolean geq(int x, int y) {
		if (x >= y) return true;
		return false;
	}
	
	public static boolean geq(int x, m_Int y) {
		if (x >= y.get()) return true;
		return false;
	}
	
	public static boolean geq(m_Int x, int y) {
		if (x.get() >= y) return true;
		return false;
	}
	
	public static boolean geq(m_Int x, m_Int y) {
		if (x.get() >= y.get()) return true;
		return false;
	}
	
	// equal to
	public static boolean eq(int x, int y) {
		if (x == y) return true;
		return false;
	}
	
	public static boolean eq(int x, m_Int y) {
		if (x == y.get()) return true;
		return false;
	}
	
	public static boolean eq(m_Int x, int y) {
		if (x.get() == y) return true;
		return false;
	}
	
	public static boolean eq(m_Int x, m_Int y) {
		if (x.get() == y.get()) return true;
		return false;
	}
	
	// not equal to
	public static boolean neq(int x, int y) {
		if (x != y) return true;
		return false;
	}
	
	public static boolean neq(int x, m_Int y) {
		if (x != y.get()) return true;
		return false;
	}
	
	public static boolean neq(m_Int x, int y) {
		if (x.get() != y) return true;
		return false;
	}
	
	public static boolean neq(m_Int x, m_Int y) {
		if (x.get() != y.get()) return true;
		return false;
	}
	
	public String toString() {
		return Integer.toString(t);
	}
	

}
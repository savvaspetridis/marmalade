package marmalade;

import jm.JMC;

public class m_String implements JMC {
	
	String s;
	
	public m_String(String x) {
		s = x;
	}
	
	public m_String(m_String x) {
		s = x.get();
	}
	
	public String get() {
		return s;
	}
	
	public void set(String x) {
		s = x;
	}
	
	public String toString() {
		return s;
	}
	
	public void print() {
		System.out.println(s);
	}
	
	// add strings
	public String add(String x, String y) {
		return x.concat(y);
	}
	
	public String add(String x, m_String y) {
		return x.concat(y.get());
	}
	
	public String add(m_String x, String y) {
		return x.get().concat(y);
	}
	
	public String add(m_String x, m_String y) {
		return x.get().concat(y.get());
	}
	
	// 
	

}
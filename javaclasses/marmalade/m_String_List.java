package marmalade;

import jm.JMC;

public class m_String_List implements JMC {
	
	String[] s;
	
	public m_String_List(String[] x) {
		s = x;
	}
	
	public m_String_List(m_String[] x) {
		s = new String[x.length];
		for (int i = 0; i < x.length; i++) {
			s[i] = x[i].get();
		}
	}
}
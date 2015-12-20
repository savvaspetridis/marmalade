package marmalade;

import jm.JMC;

public class m_Int_List implements JMC {
	
	int[] t;
	
	// constructor
	public m_Int_List(m_Int[] x) {
		t = new int[x.length];
		for (int i = 0; i < x.length; i++) {
			t[i] = x[i].get();
		}
	}
	
	public m_Int_List(int[] x) {
		this.t = x;
	}

	public int length(){
		return this.t.length;
	}
	
	public int[] getList() {
		return t;
	}
	/*
	public int get(int i) {
		return t[i];
	}*/
	
}
	
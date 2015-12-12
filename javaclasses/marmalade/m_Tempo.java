package marmalade;

import jm.JMC;
import jm.music.data.Tempo;

public class m_Tempo implements JMC {
	
	Tempo t;
	
	// constructors
	public m_Tempo(m_Int tempo) {
		t = new Tempo(tempo.get());
	}
	
	public m_Tempo(int tempo) {
		t = new Tempo(tempo);
	}
	
	// set tempo
	public void setTempo(m_Int tempo) {
		t.setTempo(tempo.get());
	}
	
	public void setTempo(int tempo) {
		t.setTempo(tempo);
	}
	
	// get tempo
	public int getTempo() {
		return (int) t.getPerMinute();
	}
	
	// print
	public String toString() {
		return Double.toString(t.getPerMinute());
	}
}
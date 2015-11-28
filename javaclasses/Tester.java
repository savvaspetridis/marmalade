import jm.music.data.Note;
import jm.util.*;


public class Tester {
	
	public static void main (String[] args) {
		Note[] n = new Note[5];
		n[0] = new Note(55, 1.0);
		n[1] = new Note(56, 1.0);
		n[2] = new Note(57, 1.0);
		n[3] = new Note(58, 1.0);
		n[4] = new Note(59, 1.0);
		
		// test measure
		//Measure m = new Measure(n);
		//m.play();
		//m.print();
		
		// test phrase
		Note[][] p = new Note[3][5];
		p[0] = n;
		p[1] = n;
		p[2] = n;
		/*m_Phrase pp = new m_Phrase(p);
		pp.play();
		pp.print();*/
		
		// test song
		Note[][][] s = new Note[2][3][5];
		s[0] = p;
		s[1] = p;
		Song ss = new Song(s);
		ss.play();
		ss.print();
		
	}
}
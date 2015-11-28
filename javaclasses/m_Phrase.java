import jm.music.data.Part;
import jm.music.data.Phrase;
import jm.music.data.Note;
import jm.util.*;

public class m_Phrase {
	
	Part p;
	
	// constructor
	public m_Phrase(Note[][] array) {
		Phrase[] phr = new Phrase[array.length];
		for (int i = 0; i < array.length; i++) {
			phr[i] = new Phrase(array[i]);
		}
		p = new Part(phr);
	}

	// play measure
	public void play() {
		Play.midi(p);
	}

	// print out measure
	public void print() {
		System.out.println(p);
	}

	// create midi file
	public void output_midi() {
		Write.midi(p);
	}

}
import jm.music.data.Phrase;
import jm.music.data.Note;
import jm.util.*;

public class Measure {
	
	Phrase p;
	
	// constructor
	public Measure(Note[] array) {
		p = new Phrase();
		p.addNoteList(array);
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
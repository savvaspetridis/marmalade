import jm.music.data.Score;
import jm.music.data.Part;
import jm.music.data.Phrase;
import jm.music.data.Note;
import jm.util.*;

public class Song {
	
	Score s;
	
	// constructor
	public Song(Note[][][] array) {
		Part[] p = new Part[array.length];

		for (int i = 0; i < array.length; i++) {	
			Phrase[] phr = new Phrase[array[i].length];
			for (int j = 0; j < array[i].length; j++) {
				phr[j] = new Phrase(array[i][j]);
			}
			p[i] = new Part(phr);
		}
		s = new Score(p);
	}

	// play measure
	public void play() {
		Play.midi(s);
	}

	// print out measure
	public void print() {
		System.out.println(s);
	}

	// create midi file
	public void output_midi() {
		Write.midi(s);
	}

}
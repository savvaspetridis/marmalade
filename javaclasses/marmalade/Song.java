package marmalade;

import jm.music.data.Score;
import jm.music.data.Part;
import jm.music.data.Phrase;
import jm.music.data.Note;
import jm.util.*;
import jm.JMC;

public class Song implements JMC {

	Score s;

	// constructor
	public Song(Note[][][] array) {
		Part[] p = new Part[array.length];

		for (int i = 0; i < array.length; i++) {	
			Phrase[] phr = new Phrase[array[i].length];
			for (int j = 0; j < array[i].length; j++) {
				phr[j] = new Phrase(array[i][j]);
			}
			p[i] = new Part(phr, "PART", 0, i);
		}
		s = new Score(p);
	}

	// set part
	public void setPart(Part p, int i) {
		s.insertPart(p, i);
	}
	
	// get part
	public Part getPart(int i) {
		return s.getPart(i);
	}
	
	
	// play measure
	public void play() {
		Play.midi(s);
	}

	// print out measure
	public void print() {
		System.out.println(s);
	}

	// create midi file - name provided
	public void output_midi(String filename) {
		Write.midi(s, filename);
	}
	
	// set instrument for a part
	public void setInstrument(int instrument, int part) {
		s.getPart(part).setInstrument(instrument);
	}

	// get instrument for a part
	public int getInstrument(int part) {
		return s.getPart(part).getInstrument();
	}
	
	// set tempo
	public void setTempo(int tempo) {
		s.setTempo((double) tempo);
	}
	
	// get tempo
	public int getTempo() {
		return (int) s.getTempo();
	}
	
	// set time sig
	public void setTimesig(int num, int den) {
		s.setTimeSignature(num, den);
	}
	
	// get time sig - numerator
	public int getTimesigNum() {
		return s.getNumerator();
	}
	
	// get time sig - denominator
	public int getTimesigDenom() {
		return s.getDenominator();
	}


}
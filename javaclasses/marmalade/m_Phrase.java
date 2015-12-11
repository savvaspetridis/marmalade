package marmalade;

import jm.music.data.Part;
import jm.music.data.Phrase;
import jm.music.data.Note;
import jm.util.*;
import jm.JMC;

public class m_Phrase implements JMC {

	Part p;

	// constructor
	public m_Phrase(m_Note[][] array) {
		
		Note[][] a = new Note[array.length][array[0].length];
		for (int i = 0; i < a.length; i++) {
			for (int j = 0; j < a[i].length; j++) {
				a[i][j] = array[i][j].toNote();
			}
		}
		Phrase[] phr = new Phrase[a.length];
		for (int i = 0; i < a.length; i++) {
			phr[i] = new Phrase(a[i]);
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

	// create midi file - name provided
	public void output_midi(String filename) {
		Write.midi(p, filename);
	}
	
	// set instrument
	public void setInstrument(int instrument) {
		p.setInstrument(instrument);
	}

	// get instrument
	public int getInstrument() {
		return p.getInstrument();
	}

	// set tempo
	public void setTempo(int tempo) {
		p.setTempo((double) tempo);
	}
	
	// get tempo
	public int getTempo() {
		return (int) p.getTempo();
	}
	
	// set time sig
	public void setTimesig(int num, int den) {
		p.setNumerator(num);
		p.setDenominator(den);
	}
	
	// get time sig - numerator
	public int getTimesigNum() {
		return p.getNumerator();
	}
	
	// get time sig - denominator
	public int getTimesigDenom() {
		return p.getDenominator();
	}
}
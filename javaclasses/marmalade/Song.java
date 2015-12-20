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
	public Song(m_Phrase[] array, int tempo) {
		Part[] p = new Part[array.length];
		for (int i = 0; i < p.length; i++) {
			p[i] = array[i].getObj();
		}
		s = new Score(p);
		setTempo(tempo);
	}
	
	public Song(m_Phrase[] array, m_Int tempo) {
		Part[] p = new Part[array.length];
		for (int i = 0; i < p.length; i++) {
			p[i] = array[i].getObj();
		}
		s = new Score(p);
		setTempo(tempo.get());
	}
	
	// return part
	public Score getObj() {
		return s;
	}
	
	// return m_Phrase
	public m_Phrase get(int i) {
		Part p = s.getPart(i);
		m_Phrase mp = new m_Phrase(p);
		return mp;
	}
	
	/*
	// constructor
	public Song(m_Note[][][] array) {
		Note[][][] a = new Note[array.length][array[0].length][array[0][0].length];
		for (int i = 0; i < a.length; i++) {
			for (int j = 0; j < a[i].length; j++) {
				for (int k = 0; k < a[i][i].length; k++) {
					a[i][j][k] = array[i][j][k].toNote();
				}
			}
		}

		Part[] p = new Part[a.length];

		for (int i = 0; i < a.length; i++) {	
			Phrase[] phr = new Phrase[a[i].length];
			for (int j = 0; j < a[i].length; j++) {
				phr[j] = new Phrase(a[i][j]);
			}
			p[i] = new Part(phr, "PART", 0, i);
		}
		s = new Score(p);
	}
	
	// constructor with tempo
	public Song(m_Note[][][] array, int tempo) {
		Note[][][] a = new Note[array.length][array[0].length][array[0][0].length];
		for (int i = 0; i < a.length; i++) {
			for (int j = 0; j < a[i].length; j++) {
				for (int k = 0; k < a[i][i].length; k++) {
					a[i][j][k] = array[i][j][k].toNote();
				}
			}
		}

		Part[] p = new Part[a.length];

		for (int i = 0; i < a.length; i++) {	
			Phrase[] phr = new Phrase[a[i].length];
			for (int j = 0; j < a[i].length; j++) {
				phr[j] = new Phrase(a[i][j]);
			}
			p[i] = new Part(phr, "PART", 0, i);
		}
		s = new Score(p);
		setTempo(tempo);
	}
	
	public Song(m_Note[][][] array, m_Int tempo) {
		Note[][][] a = new Note[array.length][array[0].length][array[0][0].length];
		for (int i = 0; i < a.length; i++) {
			for (int j = 0; j < a[i].length; j++) {
				for (int k = 0; k < a[i][i].length; k++) {
					a[i][j][k] = array[i][j][k].toNote();
				}
			}
		}

		Part[] p = new Part[a.length];

		for (int i = 0; i < a.length; i++) {	
			Phrase[] phr = new Phrase[a[i].length];
			for (int j = 0; j < a[i].length; j++) {
				phr[j] = new Phrase(a[i][j]);
			}
			p[i] = new Part(phr, "PART", 0, i);
		}
		s = new Score(p);
		setTempo(tempo);
	}
	*/

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

	// toString
	public String toString() {
		return s.toString();
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
	
	// set tempo
	public void setTempo(m_Int tempo) {
		s.setTempo((double) tempo.get());
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
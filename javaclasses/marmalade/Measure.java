package marmalade;

import jm.music.data.Phrase;
import jm.midi.event.TimeSig;
import jm.music.data.Note;
import jm.util.*;
import jm.JMC;

public class Measure implements JMC {
	
	Phrase p;
	
	public Measure(Phrase p) {
		this.p = p;
	}
	
	// constructor
	public Measure(m_Note[] array) {
		p = new Phrase();
		Note[] a = new Note[array.length];
		for (int i = 0; i < a.length; i++) {
			a[i] = array[i].getObj();
		}
		p.addNoteList(a);
	}
	
	// constructor with time sig
	public Measure(m_Note[] array, TimeSig t) {
		p = new Phrase();
		Note[] a = new Note[array.length];
		for (int i = 0; i < a.length; i++) {
			a[i] = array[i].getObj();
		}
		p.addNoteList(a);
		setTimesig(t.getNumerator(), t.getDenominator());
	}

	
	// return phrase
	public Phrase getObj() {
		return p;
	}
	
	public m_Note get(int i) {
		Note n = p.getNote(i);
		m_Note m = new m_Note(n);
		return m;
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
	
	public void setTempo(m_Int tempo) {
		p.setTempo((double) tempo.get());
	}
	
	public void setTempo(m_Tempo tempo) {
		p.setTempo((double) tempo.getTempo());
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
	
	public void setTimesig(int num, m_Int den) {
		p.setNumerator(num);
		p.setDenominator(den.get());
	}
	
	public void setTimesig(m_Int num, int den) {
		p.setNumerator(num.get());
		p.setDenominator(den);
	}
	
	public void setTimesig(m_Int num, m_Int den) {
		p.setNumerator(num.get());
		p.setDenominator(den.get());
	}
	
	// get time sig - numerator
	public int getTimesigNum() {
		return p.getNumerator();
	}
	
	// get time sig - denominator
	public int getTimesigDenom() {
		return p.getDenominator();
	}
	
	// tostring
	public String toString() {
		return p.toString();
	}
}
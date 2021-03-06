package marmalade;

import jm.music.data.Part;
import jm.music.data.Phrase;
import jm.music.data.Note;
import jm.util.*;
import jm.JMC;

public class m_Phrase implements JMC {

	public Part p;
	
	public m_Phrase(Part p) {
		this.p = p.copy();
	}
	
	// constructor
	public m_Phrase(Measure[] array, int instrument) {
		Phrase[] phr = new Phrase[array.length];
		for (int i = 0; i < phr.length; i++) {
			phr[i] = array[i].getObj();
		}
		p = new Part(phr);
		setInstrument(instrument);
	}
	
	public m_Phrase(Measure[] array, m_Int instrument) {
		Phrase[] phr = new Phrase[array.length];
		for (int i = 0; i < phr.length; i++) {
			phr[i] = array[i].getObj();
		}
		p = new Part(phr);
		setInstrument(instrument.get());
	}
	
	// return part
	public Part getObj() {
		return p;
	}
	
	public int length(){
		return this.p.length();
	}

	// return Measure
	public Measure get(int i) {
		Phrase phr = p.getPhrase(i);
		Measure m = new Measure(phr);
		return m;
	}

	public void set_Measure(int n, Measure m_rep){
		Phrase [] curr_phrases = p.getPhraseArray();


		curr_phrases[n] = m_rep.p;



		p = new Part(curr_phrases);

	}
	
	/*// constructor
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
	
	// constructor with instrument
	public m_Phrase(m_Note[][] array, int instrument) {
		
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
		setInstrument(instrument);
	}
	*/
	
	// play measure
	public void play() {
		Play.midi(p);
	}

	// print out measure
	public void print() {
		System.out.println(p);
	}

	// toString
	public String toString() {
		return p.toString();
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
	
	// get time sig - numerator
	public int getTimesigNum() {
		return p.getNumerator();
	}
	
	// get time sig - denominator
	public int getTimesigDenom() {
		return p.getDenominator();
	}
}
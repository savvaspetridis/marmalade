package marmalade;

import jm.music.data.Note;
import jm.util.*;
import jm.JMC;

public class m_Note implements JMC {

	Note n;
	
	// constructor
	public m_Note(int pitch, double length) {
		n = new Note(pitch, length);
	}
	
	public m_Note(m_Int pitch, double length) {
		int p = pitch.get();
		n = new Note(p, length);
	}
	
	// cast to Note
	public Note toNote() {
		return n;
	}
	
	// set duration
	public void setLength(double length) {
		n.setDuration(length);
	}
	
	// get duration
	public int getLength(){
		return (int) n.getDuration();
	}
	
	// set pitch
	public void setPitch(int pitch) {
		n.setPitch(pitch);
	}
	
	public void setPitch(m_Int pitch) {
		n.setPitch(pitch.get());
	}
	
	// get pitch
	public int getPitch() {
		return n.getPitch();
	}
	
	// play note
	public void play() {
		Play.midi(n);
	}

	// print out note
	public void print() {
		System.out.println(n);
	}
	
	// toString
	public String toString() {
		return n.toString();
	}
	
	// create midi file - name provided
	public void output_midi(String filename) {
		Write.midi(n, filename);
	}
	
}
package marmalade;

import jm.music.data.Note;
import jm.util.*;
import jm.JMC;


public class Tester implements JMC {
	
	public static void main (String[] args) {
		
		// test note
		m_Note note = new m_Note(50, 1.0);
		note.play();
		note.print();
		note.output_midi("measure file");
		note.setLength(2.0);
		System.out.println("Note length: " + note.getLength());
		note.setPitch(70);
		System.out.println("Note pitch: " + note.getPitch());
		note.play();

		// test measure
		Note[] n = new Note[5];
		n[0] = new Note(55, 1.0);
		n[1] = new Note(56, 1.0);
		n[2] = new Note(57, 1.0);
		n[3] = new Note(58, 1.0);
		n[4] = new Note(59, 1.0);
		
		Measure m = new Measure(n);
		m.play();
		m.print();
		m.output_midi("measure file");
		m.setInstrument(ACCORDION);
		System.out.println("Measure Instrument: " + m.getInstrument());
		m.setTempo(144);
		System.out.println("Measure tempo: " + m.getTempo());
		m.setTimesig(3, 4);
		System.out.println("Measure time sig num/denom: " + m.getTimesigNum() + " / " + m.getTimesigDenom());
		m.play();
		
		
		// test phrase
		Note[][] p = new Note[3][5];
		p[0] = n;
		p[1] = n;
		p[2] = n;
		m_Phrase phr = new m_Phrase(p);
		phr.play();
		phr.print();
		phr.output_midi("phrase file");
		phr.setInstrument(BANJO);
		System.out.println("Phrase Instrument: " + phr.getInstrument());
		phr.setTempo(144);
		System.out.println("Phrase tempo: " + phr.getTempo());
		phr.setTimesig(3, 4);
		System.out.println("Phrase time sig num/denom: " + phr.getTimesigNum() + " / " + phr.getTimesigDenom());
		phr.play();
		
		// test song
		Note[][][] s = new Note[2][3][5];
		s[0] = p;
		s[1] = p;
		Song song = new Song(s);
		song.play();
		song.print();
		System.out.println("channel 1: " + song.getPart(0).getChannel());
		System.out.println("channel 2: " + song.getPart(1).getChannel());
		song.output_midi("song file");
		song.setInstrument(VOICE, 0);
		System.out.println("Song Instruments: " + song.getInstrument(0) + ", " + song.getInstrument(1));
		song.setTempo(144);
		System.out.println("Song tempo: " + song.getTempo());
		song.setTimesig(3, 4);
		System.out.println("Song time sig num/denom: " + song.getTimesigNum() + " / " + song.getTimesigDenom());
		song.play();
		
	}
}
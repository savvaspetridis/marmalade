cd javaclasses
javac -cp ./jMusic1.6.4.jar:./ marmalade/m_Int.java
javac -cp ./jMusic1.6.4.jar:./ marmalade/m_String.java
javac -cp ./jMusic1.6.4.jar:./ marmalade/m_Note.java
javac -cp ./jMusic1.6.4.jar:./ marmalade/Measure.java
javac -cp ./jMusic1.6.4.jar:./ marmalade/m_Phrase.java
javac -cp ./jMusic1.6.4.jar:./ marmalade/Song.java
javac -cp ./jMusic1.6.4.jar:./ marmalade/m_Tempo.java
javac -cp ./jMusic1.6.4.jar:./ marmalade/Tester.java
jar cvf marmalade.jar marmalade/*.class
cd ..

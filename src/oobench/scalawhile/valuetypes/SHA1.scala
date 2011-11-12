package oobench.scalawhile.valuetypes



object SHA1 {
	val s_pad0x80 = Array[Byte]( 0x80.asInstanceOf[Byte] )
	val s_pad0x00 = Array[Byte]( 0x00 )

	val name = "Scala using WHILE loops with VALUE types"
	def apply() = new SHA1
}

class SHA1 {

	val m_h = new Array[Int](5)
	val m_w = new Array[Int](80)
	var m_iLenW: Int = 0
	var m_lBitCount: Long = 0


	// -----------------------------------------------------------------
	// CONSTRUCTOR
	// -----------------------------------------------------------------
	m_h(0)=0x67452301;
	m_h(1)=0xefcdab89;
	m_h(2)=0x98badcfe;
	m_h(3)=0x10325476;
	m_h(4)=0xc3d2e1f0;
	{ var i = 0; while (i < m_w.length) { m_w(i)=0; i += 1 } }

	// -----------------------------------------------------------------
	// Adds more data to the hash.
	// -----------------------------------------------------------------
	def update(bytes: Array[Byte]) { update(bytes, 0, bytes.length) }
	def update(bytes: Array[Byte], iOffset: Int, iLen: Int)
	{	// -----Read the data into W and process blocks as they get full
		var i = iOffset
		while (i < iLen)
		{	m_w(m_iLenW / 4) <<= 8;
			m_w(m_iLenW / 4) |= (bytes(i) & 0xFF);
			m_iLenW = m_iLenW + 1;
			if(m_iLenW % 64 == 0)
			{	hashBlock();
				m_iLenW = 0;
			}
			m_lBitCount += 8;
			i += 1;
		}
	}

	// -----------------------------------------------------------------
	// Complete the hash and get the final digest (resets internal state).
	// -----------------------------------------------------------------
	def digest(): Array[Byte] =
	{	val padlen = new Array[Byte](8);

		padlen(0)=((m_lBitCount >> 56) & 0xFF).asInstanceOf[Byte]
		padlen(1)=((m_lBitCount >> 48) & 0xFF).asInstanceOf[Byte]
		padlen(2)=((m_lBitCount >> 40) & 0xFF).asInstanceOf[Byte]
		padlen(3)=((m_lBitCount >> 32) & 0xFF).asInstanceOf[Byte]
		padlen(4)=((m_lBitCount >> 24) & 0xFF).asInstanceOf[Byte]
		padlen(5)=((m_lBitCount >> 16) & 0xFF).asInstanceOf[Byte]
		padlen(6)=((m_lBitCount >> 8) & 0xFF).asInstanceOf[Byte]
		padlen(7)=((m_lBitCount >> 0) & 0xFF).asInstanceOf[Byte]

		update(SHA1.s_pad0x80,0,1);

		while(m_iLenW!=56)  update(SHA1.s_pad0x00,0,1)

		update(padlen,0,8)

		// -----Create output hash
		val hashout = new Array[Byte](20);
		var i = 0
		while (i < 20)
		{	hashout(i) = (m_h(i/4) >> 24).asInstanceOf[Byte]
			m_h(i/4) <<= 8
			i += 1
		}

		return  hashout;
	}

	// -----------------------------------------------------------------
	// SHA rotate left.
	// -----------------------------------------------------------------
	private def shaRotl(iX: Int,iN: Int) = (iX<<iN) | (iX>>>(32-iN))

	private def hashBlock()
	{	var iA,iB,iC,iD,iE,iTemp: Int = 0
		var t = 16
		while (t <= 79)
		{
			m_w(t) = shaRotl (m_w(t-3) ^ m_w(t-8) ^ m_w(t-14) ^ m_w(t-16), 1);
			t += 1
		}

		iA = m_h(0);
		iB = m_h(1);
		iC = m_h(2);
		iD = m_h(3);
		iE = m_h(4);

		// -----Round 1
		t = 0
		while (t <= 19)
		{	iTemp = shaRotl(iA, 5) + (((iC^iD) & iB) ^ iD) + iE + m_w(t) + 0x5a827999;
			iE=iD;
			iD=iC;
			iC=shaRotl (iB, 30);
			iB=iA;
			iA=iTemp;
			t += 1;
		}

		// -----Round 2
		t = 20
		while (t <= 39)
		{	iTemp = shaRotl (iA, 5) + (iB^iC^iD) + iE + m_w(t) + 0x6ed9eba1;
			iE=iD;
			iD=iC;
			iC=shaRotl (iB, 30);
			iB=iA;
			iA=iTemp;
			t += 1;
		}

		// -----Round 3
		t = 40
		while (t <= 59)
		{	iTemp = shaRotl (iA, 5) + ((iB & iC) | (iD & (iB|iC))) + iE + m_w(t) + 0x8f1bbcdc;
			iE=iD;
			iD=iC;
			iC=shaRotl (iB, 30);
			iB=iA;
			iA=iTemp;
			t += 1;
		}

		// -----Round 4
		t = 60
		while (t <= 79)
		{	iTemp = shaRotl (iA, 5) + (iB^iC^iD) + iE + m_w(t) + 0xca62c1d6;
			iE = iD;
			iD = iC;
			iC = shaRotl (iB, 30);
			iB = iA;
			iA = iTemp;
			t += 1;
		}

		m_h(0)+=iA;
		m_h(1)+=iB;
		m_h(2)+=iC;
		m_h(3)+=iD;
		m_h(4)+=iE;
	}

}
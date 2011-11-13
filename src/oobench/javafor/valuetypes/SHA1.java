package oobench.javafor.valuetypes;

// Derived from ymsg.network.SHA1 downloaded from
// http://www.google.com/codesearch#0kiVfXNa3QY/trunk/yahoo/src/ymsg/network/SHA1.java&q=lang:java%20sha1
// http://yahoo9msg.googlecode.com/svn/trunk/yahoo/src/ymsg/network/SHA1.java

// *********************************************************************
// This class is a port of the SHA-1 algorithm included with GAIM, which
// is a port of the SHA-1 algorithm included with Mozilla somewhere.
//
// Donated to the jYMSG project by "phinaesgage" (Phinaes Gage) after
// Yahoo changed their protocol on 24th June 2004 to require direct
// manipulation of the SHA digest as it was being created.  This code
// enabled jYMSG to regain access to Yahoo within only a few days of
// the change.
//
// For v0.6 I (FISH) have modified the source to standardise its
// style with the rest of the code in the jYMSG project (just for
// consistency), added comments (more for my own benefit than anyone
// else's) and simplified it to remove test code and objects when
// not actually being debugged (to reduce its deployment footprint!)
// *********************************************************************
public class SHA1
{	private final int[] m_h , m_w;
	private int m_iLenW;
	private long m_lBitCount;

	private static final byte[] s_pad0x80 = { (byte)0x80 };
	private static final byte[] s_pad0x00 = { (byte)0x00 };

	// -----------------------------------------------------------------
	// CONSTRUCTOR
	// -----------------------------------------------------------------
	public SHA1()
	{	m_h = new int[5];
		m_w = new int[80];
		reset();
 	}

	// -----------------------------------------------------------------
	// Resets the internal state to its initial values.
	// -----------------------------------------------------------------
	public void reset()
	{	m_lBitCount=0;  m_iLenW=0;
		m_h[0]=0x67452301;
		m_h[1]=0xefcdab89;
		m_h[2]=0x98badcfe;
		m_h[3]=0x10325476;
		m_h[4]=0xc3d2e1f0;
		for (int i=0;i<m_w.length;i++)  m_w[i]=0;
	}

	// -----------------------------------------------------------------
	// Adds more data to the hash.
	// -----------------------------------------------------------------
	public void update(byte[] bytes)
	{	update(bytes, 0, bytes.length);
	}
	public void update(byte[] bytes,int iOffset,int iLen)
	{	// -----Read the data into W and process blocks as they get full
		for (int i = iOffset; i < iLen; i++)
		{	m_w[m_iLenW / 4] <<= 8;
			m_w[m_iLenW / 4] |= (bytes[i] & 0xFF);
			if((++m_iLenW) % 64 == 0)
			{	hashBlock();
				m_iLenW=0;
			}
			m_lBitCount += 8;
		}
	}

	// -----------------------------------------------------------------
	// Complete the hash and get the final digest (resets internal state).
	// -----------------------------------------------------------------
	private final byte[] padlen = new byte[8];
	public void digest(byte[] hashout)
	{
		if (hashout == null || hashout.length < 20) throw new IllegalArgumentException();

		padlen[0]=(byte)((m_lBitCount >> 56) & 0xFF);
		padlen[1]=(byte)((m_lBitCount >> 48) & 0xFF);
		padlen[2]=(byte)((m_lBitCount >> 40) & 0xFF);
		padlen[3]=(byte)((m_lBitCount >> 32) & 0xFF);
		padlen[4]=(byte)((m_lBitCount >> 24) & 0xFF);
		padlen[5]=(byte)((m_lBitCount >> 16) & 0xFF);
		padlen[6]=(byte)((m_lBitCount >> 8) & 0xFF);
		padlen[7]=(byte)((m_lBitCount >> 0) & 0xFF);

		update(s_pad0x80,0,1);

		while(m_iLenW!=56)  update(s_pad0x00,0,1);

		update(padlen,0,8);

		// -----Create output hash
		for (int i=0;i<20;i++)
		{	hashout[i] = (byte)(m_h[i/4] >> 24);
			m_h[i/4] <<= 8;
		}

		reset();
	}

	// -----------------------------------------------------------------
	// SHA rotate left.
	// -----------------------------------------------------------------
	private int shaRotl(int iX,int iN)
	{	return (iX<<iN) | (iX>>>(32-iN));
	}

	private void hashBlock()
	{	int iA,iB,iC,iD,iE,iTemp;
		for (int t=16;t<=79;t++)
			m_w[t] = shaRotl (m_w[t-3] ^ m_w[t-8] ^ m_w[t-14] ^ m_w[t-16], 1);

		iA = m_h[0];
		iB = m_h[1];
		iC = m_h[2];
		iD = m_h[3];
		iE = m_h[4];

		// -----Round 1
		for (int t = 0; t <= 19; t++)
		{	iTemp = shaRotl(iA, 5) + (((iC^iD) & iB) ^ iD) + iE + m_w[t] + 0x5a827999;
			iE=iD;
			iD=iC;
			iC=shaRotl (iB, 30);
			iB=iA;
			iA=iTemp;
		}

		// -----Round 2
		for (int t = 20; t <= 39; t++)
		{	iTemp = shaRotl (iA, 5) + (iB^iC^iD) + iE + m_w[t] + 0x6ed9eba1;
			iE=iD;
			iD=iC;
			iC=shaRotl (iB, 30);
			iB=iA;
			iA=iTemp;
		}

		// -----Round 3
		for (int t = 40; t <= 59; t++)
		{	iTemp = shaRotl (iA, 5) + ((iB & iC) | (iD & (iB|iC))) + iE + m_w[t] + 0x8f1bbcdc;
			iE=iD;
			iD=iC;
			iC=shaRotl (iB, 30);
			iB=iA;
			iA=iTemp;
		}

		// -----Round 4
		for (int t = 60; t <= 79; t++)
		{	iTemp = shaRotl (iA, 5) + (iB^iC^iD) + iE + m_w[t] + 0xca62c1d6;
			iE = iD;
			iD = iC;
			iC = shaRotl (iB, 30);
			iB = iA;
			iA = iTemp;
		}

		m_h[0]+=iA;
		m_h[1]+=iB;
		m_h[2]+=iC;
		m_h[3]+=iD;
		m_h[4]+=iE;
	}

}
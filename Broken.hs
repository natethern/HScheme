module Broken where
	{
	class C a;

	data D a = (C a) => MkD
		{
		f :: a
		};
	}

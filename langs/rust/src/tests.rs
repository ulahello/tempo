mod ringbuf {
    use crate::ringbuf::{Idx, RingBuf};

    #[test]
    fn push_pop() {
        const CAP: usize = 8;
        let mut buf: RingBuf<i32, CAP> = RingBuf::new();

        for i in 0..CAP {
            buf.try_push(i as _).unwrap();
        }
        assert_eq!(buf, [0, 1, 2, 3, 4, 5, 6, 7]);
        assert_eq!(buf.try_push(24), Err(24));

        assert_eq!(buf.pop(), Some(0));
        assert_eq!(buf.pop(), Some(1));
        assert_eq!(buf.pop(), Some(2));
        assert_eq!(buf, [3, 4, 5, 6, 7]);
    }

    #[test]
    fn push_clobber() {
        const CAP: usize = 8;
        let mut buf: RingBuf<i32, CAP> = RingBuf::new();

        for i in 0..CAP {
            buf.push(i as _);
        }
        assert_eq!(buf, [0, 1, 2, 3, 4, 5, 6, 7]);

        buf.push(24);
        assert_eq!(buf, [1, 2, 3, 4, 5, 6, 7, 24]);
    }

    #[test]
    fn overflow() {
        const CAP: usize = 8;
        let mut buf: RingBuf<usize, CAP> = RingBuf::new();

        for _ in 0..Idx::MAX {
            buf.try_push(4).unwrap();
            buf.try_push(9).unwrap();
            buf.try_push(16).unwrap();
            assert_eq!(buf.pop(), Some(4));
            assert_eq!(buf.pop(), Some(9));
            assert_eq!(buf.pop(), Some(16));
        }
        assert_eq!(buf, []);
    }

    #[test]
    fn clear() {
        const CAP: usize = 8;
        let mut buf: RingBuf<usize, CAP> = RingBuf::new();

        for i in 0..100 {
            buf.push(i);
            buf.push(i * 2);
            buf.push(i * 3);
            buf.pop().unwrap();
        }
        assert!(!buf.is_empty());
        buf.clear();
        assert!(buf.is_empty());
        assert_eq!(buf, []);
        assert_eq!(buf.pop(), None);
    }

    #[test]
    fn from_iter() {
        const CAP: usize = 8;
        let iter = [1, 2, 3, 4, 5, 6, 7, 8];
        let buf: RingBuf<usize, CAP> = RingBuf::from_iter(iter);
        assert!(buf.is_full());
        assert_eq!(buf, iter);
    }

    #[test]
    #[should_panic]
    fn from_iter_excess() {
        const CAP: usize = 8;
        let _: RingBuf<usize, CAP> = RingBuf::from_iter([1, 2, 3, 4, 5, 6, 7, 8, 9]);
    }

    #[test]
    fn truncate_front() {
        const CAP: usize = 8;
        let mut buf: RingBuf<usize, CAP> = RingBuf::from_iter([1, 2, 3, 4, 5, 6, 7, 8]);
        buf.truncate_front(3);
        assert_eq!(buf, [1, 2, 3]);
        buf.truncate_front(100);
        assert_eq!(buf, [1, 2, 3]);
    }

    #[test]
    fn truncate_back() {
        const CAP: usize = 8;
        let mut buf: RingBuf<usize, CAP> = RingBuf::from_iter([1, 2, 3, 4, 5, 6, 7, 8]);
        buf.truncate_back(3);
        assert_eq!(buf, [6, 7, 8]);
        buf.truncate_back(100);
        assert_eq!(buf, [6, 7, 8]);
    }

    #[test]
    fn double_ended_iter() {
        const CAP: usize = 8;
        let buf: RingBuf<usize, CAP> = RingBuf::from_iter([1, 2, 3, 4, 5, 6]);
        let mut iter = buf.iter();
        assert_eq!(Some(&1), iter.next());
        assert_eq!(Some(&6), iter.next_back());
        assert_eq!(Some(&5), iter.next_back());
        assert_eq!(Some(&2), iter.next());
        assert_eq!(Some(&3), iter.next());
        assert_eq!(Some(&4), iter.next());
        assert_eq!(None, iter.next());
        assert_eq!(None, iter.next_back());
    }

    #[test]
    fn double_ended_iter_overflow() {
        const CAP: usize = 8;
        let mut buf: RingBuf<Idx, CAP> = RingBuf::new();
        for i in 0..Idx::MAX {
            buf.try_push(i).unwrap();
            assert_eq!(buf.pop(), Some(i));
        }
        for x in [1, 2, 3, 4, 5, 6] {
            buf.try_push(x).unwrap();
        }
        let mut iter = buf.iter();
        assert_eq!(Some(&1), iter.next());
        assert_eq!(Some(&6), iter.next_back());
        assert_eq!(Some(&5), iter.next_back());
        assert_eq!(Some(&2), iter.next());
        assert_eq!(Some(&3), iter.next());
        assert_eq!(Some(&4), iter.next());
        assert_eq!(None, iter.next());
        assert_eq!(None, iter.next_back());
    }
}

mod tap {
    use crate::tap::Tapper;

    const EPS: f32 = 0.001;

    fn eps_eq(x: f32, y: f32) -> bool {
        (x - y).abs() < EPS
    }

    fn eps_eq_iter<'x, 'y>(
        mut xs: impl Iterator<Item = &'x f32>,
        mut ys: impl Iterator<Item = &'y f32>,
    ) -> bool {
        loop {
            match (xs.next(), ys.next()) {
                (Some(x), Some(y)) => {
                    if !eps_eq(*x, *y) {
                        return false;
                    }
                }
                (None, None) => return true,
                _ => return false, // different length
            }
        }
    }

    #[test]
    fn _eps_eq_iter() {
        assert!(eps_eq_iter([1.0, 2.0, 3.0].iter(), [1.0, 2.0, 3.0].iter()));
        assert!(!eps_eq_iter([1.0, 2.0].iter(), [1.0, 2.0, 3.0].iter()));
    }

    #[test]
    fn display() {
        let mut tapper = Tapper::new(10, true);
        for bpm in [120.05, 112.41, 121.105] {
            tapper.buf.push(bpm);
        }

        assert_eq!("[121.1, 112.4, 120.1]", format!("{tapper:.1}"));
        assert_eq!("[121.11, 112.41, 120.05]", format!("{tapper:.2}"));

        tapper.buf.clear();
        assert_eq!("[]", format!("{tapper}"));

        tapper.buf.push(112.76);
        assert_eq!("[112.8]", format!("{tapper:.1}"));
    }

    #[test]
    fn is_recording() {
        let mut tapper = Tapper::new(10, true);
        assert!(!tapper.is_recording());
        tapper.tap();
        assert!(tapper.is_recording());
        tapper.tap();
        assert!(tapper.is_recording());
        tapper.clear();
        assert!(!tapper.is_recording());
    }

    #[test]
    fn bpm() {
        let mut tapper = Tapper::new(10, true);
        assert!(eps_eq(0.0, tapper.bpm()));
        for (elem, new_avg) in [(23.0, 23.0), (26.0, 24.5), (29.0, 26.0), (61.0, 34.75)] {
            tapper.buf.push(elem);
            assert!(eps_eq(new_avg, tapper.bpm()));
        }
    }

    #[test]
    fn tap() {
        let mut tapper = Tapper::new(3, true);
        assert_eq!(0, tapper.count());
        tapper.tap();
        assert_eq!(0, tapper.count());
        tapper.tap();
        assert_eq!(1, tapper.count());
        tapper.tap();
        assert_eq!(2, tapper.count());
        tapper.tap();
        assert_eq!(3, tapper.count());
        tapper.tap();
        assert_eq!(3, tapper.count());
        tapper.toggle_bounded();
        tapper.tap();
        assert_eq!(4, tapper.count());
        tapper.tap();
        assert_eq!(5, tapper.count());
        tapper.toggle_bounded();
        assert_eq!(3, tapper.count());
    }

    #[test]
    fn truncate() {
        let mut tapper = Tapper::new(3, true);
        for bpm in [80.0, 70.0, 60.0] {
            tapper.buf.push(bpm);
        }
        assert!(eps_eq_iter([80.0, 70.0, 60.0].iter(), tapper.buf.iter()));
        tapper.buf.push(50.0);
        assert!(eps_eq_iter(
            [80.0, 70.0, 60.0, 50.0].iter(),
            tapper.buf.iter()
        ));
        tapper.sync_cap();
        assert!(eps_eq_iter([70.0, 60.0, 50.0].iter(), tapper.buf.iter()));
        tapper.toggle_bounded();
        tapper.buf.push(40.0);
        assert!(eps_eq_iter(
            [70.0, 60.0, 50.0, 40.0].iter(),
            tapper.buf.iter()
        ));
        tapper.sync_cap();
        assert!(eps_eq_iter(
            [70.0, 60.0, 50.0, 40.0].iter(),
            tapper.buf.iter()
        ));
        tapper.toggle_bounded();
        assert!(eps_eq_iter([60.0, 50.0, 40.0].iter(), tapper.buf.iter()));
    }

    #[test]
    fn resize() {
        let mut tapper = Tapper::new(3, true);
        for bpm in [80.0, 70.0, 60.0] {
            tapper.buf.push(bpm);
        }
        assert!(eps_eq_iter([80.0, 70.0, 60.0].iter(), tapper.buf.iter()));
        tapper.resize(2);
        assert!(eps_eq_iter([70.0, 60.0].iter(), tapper.buf.iter()));
        assert_eq!(2, tapper.capacity());
    }
}

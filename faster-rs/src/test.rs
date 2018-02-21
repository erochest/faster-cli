use std::collections::HashMap;
use super::*;

#[test]
fn test_parses_line() {
    let input = vec![
        b"0'9	1797	1	1",
        b"0'9	1803	1	1",
        b"0'9	1816	2	2",
        b"0'9	1817	1	1",
        b"0'9	1824	4	4",
        b"0'9	1835	1	1",
        b"0'9	1836	2	2",
        b"0'9	1839	8	4",
        b"0'9	1843	2	1",
        b"0'9	1846	3	3",
    ];
    let expected = vec![
        (1797, 1),
        (1803, 1),
        (1816, 2),
        (1817, 1),
        (1824, 4),
        (1835, 1),
        (1836, 2),
        (1839, 8),
        (1843, 2),
        (1846, 3),
    ];

    assert_eq!(
        expected,
        input
            .into_iter()
            .filter_map(|line| parse_line(line, 1, 2))
            .collect::<Vec<(usize, usize)>>()
    );
}

#[test]
fn test_count_items() {
    let input = vec![
        (1797, 1),
        (1803, 1),
        (1816, 2),
        (1817, 1),
        (1824, 4),
        (1835, 1),
        (1836, 2),
        (1839, 8),
        (1843, 2),
        (1846, 3),
        (1797, 1),
        (1816, 2),
        (1824, 4),
        (1836, 2),
        (1843, 2),
        (1797, 1),
        (1824, 4),
        (1843, 2),
    ];
    let expected = vec![
        (1797, 3),
        (1803, 1),
        (1816, 4),
        (1817, 1),
        (1824, 12),
        (1835, 1),
        (1836, 4),
        (1839, 8),
        (1843, 6),
        (1846, 3),
    ];
    let mut counts: HashMap<usize, usize> = HashMap::new();
    for items in input {
        count_items(&mut counts, items);
    }
    let mut actual = counts.into_iter().collect::<Vec<(usize, usize)>>();
    actual.sort();
    assert_eq!(expected, actual);
}

#[test]
fn test_nth() {
    let input: Vec<&[u8]> = vec![b"0'9", b"1839", b"8", b"4"];
    let mut input = input.into_iter();
    assert_eq!(Some(1839), parse_nth(&mut input, 1));
    assert_eq!(Some(8), parse_nth(&mut input, 2 - (1 + 1)));
}

mod parse_bytes {
    use super::super::parse_bytes;

    #[test]
    fn test_zero() {
        assert_eq!(Some(0), parse_bytes(b"0"));
    }

    #[test]
    fn test_empty() {
        assert_eq!(None, parse_bytes(b""));
    }

    #[test]
    fn test_nonnumeric() {
        assert_eq!(None, parse_bytes(b"hello"));
    }

    #[test]
    fn test_trailing() {
        assert_eq!(Some(42), parse_bytes(b"42hello"));
    }
}

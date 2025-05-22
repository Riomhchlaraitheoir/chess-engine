use std::ops::ControlFlow;

impl<T: Sized> IterExt for T where T: Iterator {}

pub trait IterExt: Iterator + Sized {
    fn map_while_inclusive<R>(
        self,
        f: impl Fn(Self::Item) -> ControlFlow<Option<R>, R>,
    ) -> impl Iterator<Item = R> {
        MapWhileInclusive {
            iterator: self,
            mapper: f,
            done: false,
        }
    }
}

struct MapWhileInclusive<I, F> {
    iterator: I,
    mapper: F,
    done: bool,
}

impl<R, I: Iterator, F: FnMut(I::Item) -> ControlFlow<Option<R>, R>> Iterator
    for MapWhileInclusive<I, F>
{
    type Item = R;

    fn next(&mut self) -> Option<Self::Item> {
        if self.done {
            return None;
        }
        match self.iterator.next().map(&mut self.mapper)? {
            ControlFlow::Continue(r) => Some(r),
            ControlFlow::Break(r) => {
                self.done = true;
                r
            }
        }
    }
}

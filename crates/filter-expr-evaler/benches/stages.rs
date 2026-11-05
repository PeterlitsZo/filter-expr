fn main() {
    tokio::runtime::Builder::new_current_thread()
        .build()
        .unwrap()
        .block_on(filter_expr_evaler::benchmark::run());
}

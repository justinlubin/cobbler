returnsExpectedContractData : ContractData -> Result String ContractData -> Expectation
returnsExpectedContractData expectedData maybeData =
    case maybeData of
        Ok data ->
            Expect.equal data expectedData

        Err error ->
            Expect.fail error

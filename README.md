# 🐄 Livestock Ownership NFT System

A blockchain-based solution for digitizing livestock ownership using NFTs on Stacks.

## 🎯 Features

- 🔒 Mint NFTs representing livestock with detailed metadata
- 📝 Track ownership transfers securely
- 💉 Update health records and veterinary checks
- 💰 Create and manage loans using livestock as collateral
- 📍 Track location and movement history

## 🚀 Getting Started

### Prerequisites

- Clarinet
- Stacks Wallet
- Node.js

### Installation

1. Clone the repository:
```bash
git clone https://github.com/yourusername/livestock-ownership-nft-system
```

2. Install dependencies:
```bash
clarinet install
```

### Usage

#### Minting New Livestock NFT
```clarity
(contract-call? .livestock-ownership-nft-system mint "Angus" u1654012800 "Healthy" "Farm A")
```

#### Transferring Ownership
```clarity
(contract-call? .livestock-ownership-nft-system transfer u1 tx-sender 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM)
```

#### Creating a Loan
```clarity
(contract-call? .livestock-ownership-nft-system create-loan u1 u1000000 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM u144)
```

## 📝 Contract Functions

- `mint`: Create new livestock NFT
- `transfer`: Transfer ownership
- `update-health-status`: Update health records
- `create-loan`: Create a loan using NFT as collateral
- `repay-loan`: Mark loan as repaid
- `get-token-metadata`: View livestock details
- `get-loan-details`: View active loans

## 🤝 Contributing

Pull requests are welcome! For major changes, please open an issue first.

## 📜 License

MIT
```


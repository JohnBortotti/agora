import React from 'react';
import { useParams } from 'react-router-dom';

const NodeDetail: React.FC = () => {
  const { nodeId } = useParams<{ nodeId: string }>();

  // Fetch node details based on nodeId
  // Placeholder content
  return (
    <div>
      <h2>Node Details</h2>
      <p>ID: {nodeId}</p>
      {/* Additional details about the node */}
    </div>
  );
};

export default NodeDetail;
